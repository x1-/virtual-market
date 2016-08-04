package com.inkenkun.x1.virtual.market.transaction

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

import akka.actor.Actor
import akka.pattern.ask
import org.joda.time.{DateTime, Duration => JodaDuration}

import com.inkenkun.x1.virtual.market.stock.Candles
import com.inkenkun.x1.virtual.market.redis.{Handler => RedisHandler}
import com.inkenkun.x1.virtual.market.user.{Contract, Contracts}

object Manager extends RedisHandler {
  
  import com.inkenkun.x1.virtual.market._
  import implicits._

  private val prefixWith: String => String = { s => "job_%s".format( s ) }

  def start( contract: Contract ) = {

    import system.dispatcher

    val adjustedContract = if ( contract.how.isMarket ) contract.copy( price = contract.unit ) else contract

    val stage = AccountsManager ? ( "stage", contract )
    stage.mapTo[Try[Boolean]].onComplete {
      case Success(s) =>
        setToRedis( prefixWith( adjustedContract.jobId ), adjustedContract.toJson )
        if ( contract.how.isMarket ) {
          TransactionManager ! adjustedContract.jobId
        }
        else {
          val now = marketNow
          val duration = new JodaDuration( now, new DateTime( contract.expiration ) )
          system.scheduler.scheduleOnce(
            duration.plus( 1000L * 30 ).getMillis millisecond,
            TransactionManager,
            contract.jobId
          )
        }
      case Failure(e) => TransactionManager ! ( e, s"manager.start.${contract.userId}" )
    }
    Await.result( stage, Duration.Inf )
  }

  def commit( jobId: String ) = {

    import system.dispatcher
    val maybeContract = getFromRedis( prefixWith( jobId ) )

    for {
      contractJson <- maybeContract
      preContract   = contractJson.parseAs[Contract]
    } yield {
      val contract =
        if ( preContract.how.isLimit && !Candles.isReached( preContract.code, preContract.price, preContract.startTime, new DateTime( preContract.expiration ) ) )
          preContract.copy(
            status = Contracts.Status.impossible
          )
        else
          preContract.copy(
            status = Contracts.Status.done
          )

      val reg = AccountsManager ? ( "commit", contract )
      reg.mapTo[Try[Boolean]].onComplete {
        case Success(s) => LogManager ! Log( "commited.", s"manager.commit.$jobId" )
        case Failure(e) => LogManager ! Log( "", s"manager.commit.$jobId", e = Some( e ) )
      }
      Await.result( reg, Duration.Inf )
    }
  }

  class JobActor extends Actor {
    def receive = {
      case ( contract: Contract ) =>
        start( contract )

      case ( jobId: String ) =>
        commit( jobId )
    }
  }
}