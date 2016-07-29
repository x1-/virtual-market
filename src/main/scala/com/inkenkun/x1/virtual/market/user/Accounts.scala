package com.inkenkun.x1.virtual.market.user

import java.util.Date
import scala.util.Try

import akka.actor.{Actor, ActorLogging}
import scalikejdbc.{SQLSyntaxSupport, WrappedResultSet}

import com.inkenkun.x1.virtual.market.mysql.{Handler => MySQLHandler}
import com.inkenkun.x1.virtual.market.redis.{Handler => RedisHandler}
import com.inkenkun.x1.virtual.market.transaction.{Account => AccType, BoS, SoL}

case class Account(
  userId          : String = "",
  userName        : String = "",
  availableCash   : BigDecimal = BigDecimal( 3000000 ),
  availableCredit : BigDecimal = BigDecimal( 6000000 ),
  balance         : BigDecimal = BigDecimal( 3000000 ),
  holdings        : List[Holding] = List.empty[Holding],
  contracted      : List[Contract] = List.empty[Contract],
  notContracted   : List[Contract] = List.empty[Contract]
) {
  def calcAvailableCash( contract: Contract ): BigDecimal = {

    val commitPrice = contract.price * contract.volume
    val maybeStock  = findStock( contract.market, contract.code )

    ( contract.bos, contract.status, contract.sol, contract.account ) match {
      case ( BoS.buy,  Contracts.Status.done,       SoL.long,  AccType.cash   ) => availableCash
      case ( BoS.buy,  Contracts.Status.notYet,     SoL.long,  AccType.cash   ) => availableCash - commitPrice
      case ( BoS.buy,  Contracts.Status.impossible, SoL.long,  AccType.cash   ) => availableCash + commitPrice

      case ( BoS.sell, Contracts.Status.done,       SoL.long,  AccType.cash   ) => availableCash + commitPrice
      case ( BoS.sell, Contracts.Status.notYet,     _,         AccType.cash   ) => availableCash
      case ( BoS.sell, Contracts.Status.impossible, _,         AccType.cash   ) => availableCash

      case ( _,        _,                           _,         AccType.credit ) => availableCredit
    }
  }

  def calcAvailableCredit( contract: Contract ): BigDecimal = {

    val commitPrice = contract.price * contract.volume
    val maybeStock  = findStock( contract.market, contract.code )

    ( contract.bos, contract.status, contract.sol, contract.account ) match {
      case ( BoS.buy,  Contracts.Status.done,       _,         AccType.credit  ) => availableCredit
      case ( BoS.buy,  Contracts.Status.notYet,     _,         AccType.credit  ) => availableCredit - commitPrice
      case ( BoS.buy,  Contracts.Status.impossible, _,         AccType.credit  ) => availableCredit + commitPrice

      case ( BoS.sell, Contracts.Status.done,       SoL.long,  AccType.credit  ) => availableCredit + commitPrice
      case ( BoS.sell, Contracts.Status.done,       SoL.short, AccType.credit ) =>
        val stock    = maybeStock.get
        val original = stock.price * contract.volume
        val profit   = original - commitPrice
        availableCredit + original + profit
      case ( BoS.sell, Contracts.Status.notYet,     _,         AccType.credit ) => availableCredit
      case ( BoS.sell, Contracts.Status.impossible, _,         AccType.credit ) => availableCredit
      case ( _,        _,                           _,         AccType.cash   ) => availableCash
    }
  }

  def calcHoldings( contract: Contract ): List[Holding] = {
    val holding = Holding (
      userId = contract.userId,
      time   = contract.expiration,
      market = contract.market,
      code   = contract.code,
      price  = contract.price,
      volume = contract.volume,
      soL    = contract.sol
    )
    if ( contract.bos.isBuy && contract.status.isDone ) {
      val change = findStock( contract.market, contract.code ) match {
        case Some( x ) =>
          holding.copy(
            price  = ( ( ( x.price * x.volume ) + ( holding.price * holding.volume ) ) / ( x.volume + holding.volume ) ).setScale( 3, BigDecimal.RoundingMode.HALF_UP ),
            volume = x.volume + holding.volume
          )
        case None =>
          holding
      }
      holdings.filterNot( h => h.market == holding.market && h.code == holding.code ) :+ change
    }
    else if ( contract.bos.isSell && contract.status.isDone ) {
      val change = findStock( holding.market, holding.code ).map { ho =>
        val diff = ho.volume - holding.volume
        if ( diff > 0 ) {
          List( ho.copy(
            volume = diff
          ) )
        }
        else
          Nil
      }
      holdings.filterNot( h => h.market == holding.market && h.code == holding.code ) ++ change.getOrElse( Nil )
    }
    else
      holdings
  }
  private def findStock( market: String, code: String ): Option[Holding] =
    holdings.find( h => h.market == market && h.code == code )

}
object Account extends SQLSyntaxSupport[Account] {
  def apply( rs: WrappedResultSet ): Account =
    new Account(
      userId          = rs.string( "user_id" ),
      userName        = rs.string( "user_name" ),
      availableCash   = rs.bigDecimal( "available_cash" ),
      availableCredit = rs.bigDecimal( "available_credit" ),
      balance         = rs.bigDecimal( "balance" )
    )
}

object Accounts extends MySQLHandler with RedisHandler {

  import com.inkenkun.x1.virtual.market.userId

  lazy val initialUsers: List[Account] = UserDao.fullFetchAll

  def retrieve( id: userId ): Account = UserDao.retrieve( id )

  private def updateStagedContract( contract: Contract ): Try[Boolean] = {
    val user    = retrieve( contract.userId )
    val newUser = user.copy(
      availableCash   = user.calcAvailableCash( contract ),
      availableCredit = user.calcAvailableCredit( contract ),
      notContracted   = user.notContracted :+ contract
    )
    UserDao.update( newUser )
  }

  private def updateContract( contract: Contract ): Try[Boolean] = {
    val user    = retrieve( contract.userId )
    val newUser = user.copy(
      availableCash   = user.calcAvailableCash( contract ),
      availableCredit = user.calcAvailableCredit( contract ),
      holdings        = user.calcHoldings( contract ),
      contracted      = user.contracted :+ contract,
      notContracted   = user.notContracted.filterNot( _.jobId == contract.jobId )
    )
    UserDao.update( newUser )
  }

  private def load(): Try[Unit] = UserDao.expand( initialUsers )

  private def add( userId: String, userName: String ): Try[Long] = {
    UserDao.add( Account(
      userId   = userId,
      userName = userName
    ) )
  }

  private def reset( userId: userId ): Try[Unit] = UserDao.reset( userId )


  class ManagerActor extends Actor with ActorLogging {
    def receive = {
      case "load" =>
        load().recover {
          case e: Throwable => log.error( e, "manager.load" )
        }

      case ( "add", userId: String, userName: String ) =>
        add( userId, userName ).recover {
          case e: Throwable => log.error( e, "manager.add" )
        }

      case ( "reset", userId: String ) =>
        reset( userId ).recover {
          case e: Throwable => log.error( e, "manager.reset" )
        }

      case ( "stage", contract: Contract ) =>
        sender ! updateStagedContract( contract )

      case ( "commit", contract: Contract ) =>
        updateContract( contract ).recover {
          case e: Throwable => log.error( e, "manager.commit" )
        }

    }
  }
}
