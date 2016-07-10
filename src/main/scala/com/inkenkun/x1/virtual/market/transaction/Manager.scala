package com.inkenkun.x1.virtual.market.transaction

import scala.util.Random

import akka.actor.{Actor, ActorLogging}
import org.joda.time.Duration

import com.inkenkun.x1.virtual.market.stock.Candles
import com.inkenkun.x1.virtual.market.user.{Accounts, Contract, Contracts, Holding}

object Manager {

  import com.inkenkun.x1.virtual.market._
  import implicits._

  def jobId = Random.nextInt( 99999999 ).toString

  def start( jobId: String, contract: Contract ) = {

    val adjustedContract = if ( contract.how.isMarket ) contract.copy( price = contract.unit ) else contract

    AccountsManager ! ( "yet", contract )

    redis.Handler.set( jobId, adjustedContract.toJson )

    if ( contract.how.isMarket ) {
      TransactionManager ! jobId
    }
    else {
      val now = marketNow
      val duration = new Duration( now, contract.expiration )
      system.scheduler.scheduleOnce(
        duration.plus( 1000L * 30 ).getMillis,
        TransactionManager,
        jobId
      )
    }
  }

  def commit( jobId: String ) = {

    val maybeContract = redis.Handler.get( jobId )
    for {
      contractJson <- maybeContract
      preContract   = contractJson.parseAs[Contract]
    } yield {
      val contract = if ( preContract.how.isLimit && !Candles.isReached( preContract.code, preContract.startTime, preContract.expiration ) ) {
        preContract.copy(
          status = Contracts.Status.impossible
        )
      }
      else
        preContract

      val commitPrice = contract.price * contract.number
      val user = Accounts.retrieve( contract.userId )

      val cash   = availableCash( contract, user )
      val credit = availableCredit( contract, user )

      val holdings = user.holdings + Holding (
        time   = contract.expiration.toDate,
        market = contract.stock.get.market,
        code   = contract.code,
        price  = contract.price,
        volume = contract.number
      )
      val newUser = user.copy(

      )

      AccountsManager ! ( "update", user )
    }
  }

  private def availableCash( contract: Contract, acc: user.Account ): BigDecimal = {

    val commitPrice = contract.price * contract.number

    ( contract.bos.isBuy, contract.account.isCash ) match {
      case ( true, true )  => acc.availableCash - commitPrice
      case ( false, true ) => acc.availableCash + commitPrice
      case ( _, false )    => acc.availableCash
    }
  }

  private def availableCredit( contract: Contract, acc: user.Account ): BigDecimal = {

    val commitPrice = contract.price * contract.number

    ( contract.bos.isBuy, contract.account.isCredit ) match {
      case ( true, true )  => acc.availableCredit - commitPrice
      case ( false, true ) => acc.availableCredit + commitPrice
      case ( _, false )    => acc.availableCredit
    }
  }

  class JobActor extends Actor with ActorLogging {
    def receive = {
      case ( jobId: String, contract: Contract ) =>
        start( jobId, contract )

      case ( jobId: String ) =>
        commit( jobId )
    }
  }
}