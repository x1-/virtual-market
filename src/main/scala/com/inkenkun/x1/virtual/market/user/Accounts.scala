package com.inkenkun.x1.virtual.market.user

import scala.util.Try

import akka.actor.{Actor, ActorLogging}
import org.joda.time.DateTime
import scalikejdbc.{SQLSyntaxSupport, WrappedResultSet}

import com.inkenkun.x1.virtual.market.mysql.{Handler => MySQLHandler}
import com.inkenkun.x1.virtual.market.redis.{Handler => RedisHandler}
import com.inkenkun.x1.virtual.market.stock.Candles
import com.inkenkun.x1.virtual.market.transaction.{BoS, SoL, Account => AccType}

case class Account(
  userId          : String = "",
  userName        : String = "",
  availableCash   : BigDecimal = BigDecimal( 3000000 ),
  availableCredit : BigDecimal = BigDecimal( 6000000 ),
  balance         : BigDecimal = BigDecimal( 3000000 ),
  loan            : BigDecimal = BigDecimal( 0 ),
  holdings        : List[Holding] = List.empty[Holding],
  contracted      : List[Contract] = List.empty[Contract],
  notContracted   : List[Contract] = List.empty[Contract]
) {

  def reBalance( now: DateTime ): Account = {

    val performance = holdings.foldLeft( BigDecimal(0) ) { ( acc, holding ) =>
      val stock = Candles.latest( holding.code, now.toDate )
      acc + stock.fold( BigDecimal(0) )( s => s.close * holding.volume )
    }
    val cash = if ( loan < 0 ) availableCash - loan else availableCash
    val adv  = if ( loan < 0 ) BigDecimal(0) else loan

    this.copy(
      availableCash = cash,
      balance       = performance + cash,
      loan          = adv
    )
  }


  private[user] def calcAvailableCash( contract: Contract ): BigDecimal = {

    val commitPrice = contract.price * contract.volume

    ( contract.account, contract.bos, contract.status, contract.sol ) match {
      case ( AccType.credit, _,        _,                           _          ) => availableCash
      case ( AccType.cash,   BoS.buy,  Contracts.Status.done,       SoL.long   ) => availableCash
      case ( AccType.cash,   BoS.buy,  Contracts.Status.notYet,     SoL.long   ) => availableCash - commitPrice
      case ( AccType.cash,   BoS.buy,  Contracts.Status.impossible, SoL.long   ) => availableCash + commitPrice

      case ( AccType.cash,   BoS.sell, Contracts.Status.done,       SoL.long   ) => availableCash + commitPrice
      case ( AccType.cash,   BoS.sell, Contracts.Status.notYet,     _          ) => availableCash
      case ( AccType.cash,   BoS.sell, Contracts.Status.impossible, _          ) => availableCash

    }
  }

  private[user] def calcAvailableCredit( contract: Contract ): BigDecimal = {

    val commitPrice = contract.price * contract.volume
    val maybeStock  = findStock( contract.market, contract.code )

    ( contract.account, contract.bos, contract.status, contract.sol ) match {
      case ( AccType.cash,    _,        _,                           _         ) => availableCredit
      case ( AccType.credit,  BoS.buy,  Contracts.Status.done,       _         ) => availableCredit
      case ( AccType.credit,  BoS.buy,  Contracts.Status.notYet,     _         ) => availableCredit - commitPrice
      case ( AccType.credit,  BoS.buy,  Contracts.Status.impossible, _         ) => availableCredit + commitPrice

      case ( AccType.credit,  BoS.sell, Contracts.Status.done,       SoL.long  ) => availableCredit + commitPrice
      case ( AccType.credit,  BoS.sell, Contracts.Status.done,       SoL.short ) =>
        val stock    = maybeStock.get
        val original = stock.price * contract.volume
        val profit   = original - commitPrice
        availableCredit + original + profit
      case ( AccType.credit,  BoS.sell, Contracts.Status.notYet,     _         ) => availableCredit
      case ( AccType.credit,  BoS.sell, Contracts.Status.impossible, _         ) => availableCredit
    }
  }

  private[user] def calcLoan( contract: Contract ): BigDecimal = {

    val commitPrice = contract.price * contract.volume

    ( contract.account, contract.bos, contract.status, contract.sol ) match {
      case ( AccType.cash,    _,        _,                           _         ) => loan
      case ( AccType.credit,  _,        Contracts.Status.notYet,     _         ) => loan
      case ( AccType.credit,  _,        Contracts.Status.impossible, _         ) => loan

      case ( AccType.credit,  BoS.buy,  Contracts.Status.done,       _         ) => loan + commitPrice
      case ( AccType.credit,  BoS.sell, Contracts.Status.done,       _         ) => loan - commitPrice
    }
  }

  private[user] def calcHoldings( contract: Contract ): List[Holding] = {
    val holding = Holding (
      userId = contract.userId,
      time   = contract.expiration,
      market = contract.market,
      code   = contract.code,
      price  = contract.price,
      volume = contract.volume,
      soL    = contract.sol,
      id     = None
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

  import com.inkenkun.x1.virtual.market._

  lazy val initialUsers: List[Account] = UserDao.fullFetchAll

  def retrieve( id: userId ): Account = UserDao.retrieve( id )
  def save(): Try[Unit] = UserDao.save

  private def stageContract( contract: Contract ): Try[Boolean] = {
    val user    = retrieve( contract.userId )
    val newUser = user.copy(
      availableCash   = user.calcAvailableCash( contract ),
      availableCredit = user.calcAvailableCredit( contract ),
      notContracted   = user.notContracted :+ contract
    )
    UserDao.update( newUser )
  }

  private def commitContract( contract: Contract ): Try[Boolean] = {
    val user    = retrieve( contract.userId )
    val newUser = user.copy(
      availableCash   = user.calcAvailableCash( contract ),
      availableCredit = user.calcAvailableCredit( contract ),
      holdings        = user.calcHoldings( contract ),
      contracted      = user.contracted :+ contract,
      notContracted   = user.notContracted.filterNot( _.jobId == contract.jobId )
    ).reBalance( marketNow )
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

  private def resetNotContract( userId: userId ): Try[Boolean] = {
    val user    = retrieve( userId )
    val backedCash = user.notContracted
        .filter( contract => contract.account.isCash )
        .foldLeft( BigDecimal(0) ) { ( acc, contract ) => contract.price * contract.volume + acc }
    val backedCredit = user.notContracted
        .filter( contract => contract.account.isCredit )
        .foldLeft( BigDecimal(0) ) { ( acc, contract ) => contract.price * contract.volume + acc }

    val newUser = user.copy(
      availableCash   = user.availableCash   + backedCash,
      availableCredit = user.availableCredit + backedCredit,
      notContracted   = List.empty[Contract]
    )
    UserDao.update( newUser )
  }


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

      case ( "reset/notcontracted", userId: String ) =>
        resetNotContract( userId ).recover {
          case e: Throwable => log.error( e, "manager.reset/notcontracted" )
        }

      case ( "stage", contract: Contract ) =>
        sender ! stageContract( contract )

      case ( "commit", contract: Contract ) =>
        sender ! commitContract( contract )

    }
  }
}
