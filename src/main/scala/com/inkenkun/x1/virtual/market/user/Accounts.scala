package com.inkenkun.x1.virtual.market.user

import java.util.Date
import scala.collection.mutable

import akka.actor.{Actor, ActorLogging}

import com.inkenkun.x1.virtual.market.transaction.{Account => AccType, BoS, SoL}
import com.inkenkun.x1.virtual.market.redis

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

    val commitPrice = contract.price * contract.number
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

    val commitPrice = contract.price * contract.number
    val maybeStock  = findStock( contract.market, contract.code )

    ( contract.bos, contract.status, contract.sol, contract.account ) match {
      case ( BoS.buy,  Contracts.Status.done,       _,         AccType.credit  ) => availableCredit
      case ( BoS.buy,  Contracts.Status.notYet,     _,         AccType.credit  ) => availableCredit - commitPrice
      case ( BoS.buy,  Contracts.Status.impossible, _,         AccType.credit  ) => availableCredit + commitPrice

      case ( BoS.sell, Contracts.Status.done,       SoL.long,  AccType.credit  ) => availableCredit + commitPrice
      case ( BoS.sell, Contracts.Status.done,       SoL.short, AccType.credit ) =>
        val stock    = maybeStock.get
        val original = stock.price * contract.number
        val profit   = original - commitPrice
        availableCredit + original + profit
      case ( BoS.sell, Contracts.Status.notYet,     _,         AccType.credit ) => availableCredit
      case ( BoS.sell, Contracts.Status.impossible, _,         AccType.credit ) => availableCredit
      case ( _,        _,                           _,         AccType.cash   ) => availableCash
    }
  }

  def calcHoldings( contract: Contract ): List[Holding] = {
    val holding = Holding (
      time   = contract.expiration.toDate,
      market = contract.market,
      code   = contract.code,
      price  = contract.price,
      volume = contract.number,
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

case class Holding (
  time   : Date,
  market : String,
  code   : String,
  price  : BigDecimal,
  volume : Int,
  soL    : SoL
)

object Accounts{

  import com.inkenkun.x1.virtual.market.userId
  import com.inkenkun.x1.virtual.market.implicits._

  val saveKey = "users"
  val users   = mutable.Map.empty[userId, Account]

  lazy val initialUsers: List[Account] = {
    val maybeUsers = redis.Handler.get( saveKey )
    maybeUsers map( _.parseAs[List[Account]] ) getOrElse List.empty[Account]
  }

  def retrieve( id: userId ): Account = users.getOrElse( id, Account() )

  private def updateStagedContract( contract: Contract ): Unit = synchronized {
    for {
      user   <- users.get( contract.userId )
    } yield {
      val newUser = user.copy(
        availableCash   = user.calcAvailableCash( contract ),
        availableCredit = user.calcAvailableCredit( contract ),
        notContracted   = user.notContracted :+ contract
      )
      users.update( user.userId, newUser )
      redis.Handler.set( saveKey, users.values.toList.toJson )
    }
  }
  private def updateContract( contract: Contract ): Unit = synchronized {
    for {
      user   <- users.get( contract.userId )
    } yield {
      val newUser = user.copy(
        availableCash   = user.calcAvailableCash( contract ),
        availableCredit = user.calcAvailableCredit( contract ),
        holdings        = user.calcHoldings( contract ),
        contracted      = user.contracted :+ contract,
        notContracted   = user.notContracted.filterNot( _.jobId == contract.jobId )
      )
      users.update( user.userId, newUser )
      redis.Handler.set( saveKey, users.values.toList.toJson )
    }
  }

  private def load(): Unit = synchronized {
    initialUsers.map { acc =>
      users += ( acc.userId -> acc )
    }
  }
  private def add( userId: String, userName: String ): Unit = synchronized {
    users += ( userId -> Account(
      userId   = userId,
      userName = userName
    ) )
  }
  private def reset( userId: String ): Unit = synchronized {
    users.get( userId ) match {
      case Some(a) =>
        users.update( userId, Account(
          userId   = a.userId,
          userName = a.userName
        ) )
      case None => {}
    }
  }

  class ManagerActor extends Actor with ActorLogging {
    def receive = {
      case "load" =>
        if ( users.nonEmpty ) {
          load()
        }
      case ( "add", userId: String, userName: String ) =>
        add( userId, userName )

      case ( "reset", userId: String ) =>
        reset( userId )

      case ( "stage", contract: Contract ) =>
        updateStagedContract( contract )

      case ( "commit", contract: Contract ) =>
        updateContract( contract )

    }
  }
}
