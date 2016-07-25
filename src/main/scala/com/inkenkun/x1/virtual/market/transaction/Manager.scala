package com.inkenkun.x1.virtual.market.transaction

import scala.concurrent.duration._

import akka.actor.{Actor, ActorLogging}
import org.joda.time.Duration

import com.inkenkun.x1.virtual.market.stock.Candles
import com.inkenkun.x1.virtual.market.user.{Contract, Contracts}

object Manager {

  import com.inkenkun.x1.virtual.market._
  import implicits._

  def start( contract: Contract ) = {

    val adjustedContract = if ( contract.how.isMarket ) contract.copy( price = contract.unit ) else contract

    AccountsManager ! ( "stage", contract )
    redis.Handler.set( adjustedContract.jobId, adjustedContract.toJson )

    if ( contract.how.isMarket ) {
      TransactionManager ! contract.jobId
    }
    else {
      import system.dispatcher
      val now = marketNow
      val duration = new Duration( now, contract.expiration )
      system.scheduler.scheduleOnce(
        duration.plus( 1000L * 30 ).getMillis millisecond,
        TransactionManager,
        contract.jobId
      )
    }
  }

  def commit( jobId: String ) = {

    val maybeContract = redis.Handler.get( jobId )

    for {
      contractJson <- maybeContract
      preContract   = contractJson.parseAs[Contract]
    } yield {
      val contract =
        if ( preContract.how.isLimit && !Candles.isReached( preContract.code, preContract.price, preContract.startTime, preContract.expiration ) )
          preContract.copy(
            status = Contracts.Status.impossible
          )
        else
          preContract

      AccountsManager ! ( "commit", contract )
    }
  }

  class JobActor extends Actor with ActorLogging {
    def receive = {
      case ( contract: Contract ) =>
        start( contract )

      case ( jobId: String ) =>
        commit( jobId )
    }
  }
}