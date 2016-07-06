package com.inkenkun.x1.virtual.market

import akka.actor.Actor
import spray.http.MediaTypes._
import spray.routing._
import spray.routing.RejectionHandler.Default

import com.inkenkun.x1.virtual.market.stock._
import com.inkenkun.x1.virtual.market.transaction._
import com.inkenkun.x1.virtual.market.user.Accounts

class ServiceActor extends Actor with Service {

  def actorRefFactory = context

  def receive = runRoute(route)
}

trait Service extends HttpService {

  import implicits._

  val route = {
    path( "time" ) {
      get {
        respondWithMediaType( `application/json` ) {
          complete( s"""{ "time": "${ marketTime( baseTime, System.currentTimeMillis )( marketStart ).toString( timestampFormat ) }" }""" )
        }
      }
    } ~
    path( "candles" / "fetch" ) {
      get {
        parameters( 'tick, 'start, 'end ) { ( tick, start, end ) =>
          val startDate = dateFormat.parseDateTime( start )
          val endDate   = dateFormat.parseDateTime( end )
          CandleFetcher ! ( Tick( Some( tick ) ), startDate, endDate )
          respondWithMediaType( `text/html` ) {
            complete(
              <html><head></head>
                <body>
                  <p>message is proceccing... You can see status at below.</p>
                  <a href="/candles/status">status</a>
                </body>
              </html>
            )
          }
        }
      }
    } ~
    path( "candles" / "status" ) {
      get {
        respondWithMediaType(`text/html`) {
          complete(
            <html>
              <head></head>
              <body>
                <p>Status: { Candles.status }</p>
              </body>
            </html>
          )
        }
      }
    } ~
    path( "stocks" / "available" ) {
      get {
        respondWithMediaType(`application/json`) {
          complete( Stocks.values.toJson )
        }
      }
    } ~
    path( "user" / "load" ) {
      get {
        respondWithMediaType( `application/json` ) {
          AccountsManager ! "load"
          complete(
            <html>
              <head></head>
              <body>
                <p>Status: loading</p>
              </body>
            </html>
          )
        }
      }
    } ~
    path( "user" / "info" ) {
      get {
        parameters( 'id ) { id =>
          val account = Accounts.retrieve( id )
          respondWithMediaType( `application/json` ) {
            complete( account.toJson )
          }
        }
      }
    } ~
    path( "user" / "contract" / "notyet" ) {
      get {
        parameters( 'id ) { id =>
          respondWithMediaType( `application/json` ) {
            complete( s"" )
          }
        }
      }
    } ~
    path( "user" / "contract" / "done" ) {
      get {
        parameters( 'id ) { id =>
          respondWithMediaType( `application/json` ) {
            complete( s"" )
          }
        }
      }
    } ~
    path( "buy" ) {
      get {
        parameters( 'id, 'code, 'how ? "market", 'sol ? "long", 'number.as[Int] ? 100, 'expiration.?, 'account ? "cash" ) { ( id, code, how, sol, number, expiration, account ) =>
          val now       = marketTime( baseTime, System.currentTimeMillis )( marketStart )
          val howType   = How( how )
          val solTypw   = SoL( sol )
          val accType   = Account( account )
          val exprDate  = expiration map timestampFormat.parseDateTime _ getOrElse now


          val message = if ( solTypw == SoL.short && accType == Account.cash ) {
            "Error: 空売りは信用取引でしかできません。"
          } else if ( exprDate.isBefore( now ) ) {
            s"Error: 有効期限が過ぎています。 現在時刻:${ now.toString( timestampFormat ) }"
          } else {
            "Success: processing"
          }
          respondWithMediaType( `application/json` ) {
            complete(
              s"""{"message":"$message","jobId":""}""" )
          }
        }
      }
    } ~
    path( "sell" ) {
      get {
        parameters( 'id, 'code, 'how ? "market", 'sol ? "long", 'number.as[Int] ? 100, 'expiration.? ) { ( id, code, how, sol, number, expiration ) =>
          respondWithMediaType( `application/json` ) {
            complete( s"" )
          }
        }
      }
    } ~
    path( "price" ) {
      get {
        parameters( 'code, 'start.?, 'end.?, 'tick.? ) { ( code, start, end, tick ) =>
          val now       = marketTime( baseTime, System.currentTimeMillis )( marketStart )
          val tickType  = Tick( tick )
          val tickMin   = Tick.tick2minutes( tickType )
          val startDate = start map timestampFormat.parseDateTime _ getOrElse now.minusMinutes( tickMin )
          val endDate   = end map timestampFormat.parseDateTime _ getOrElse now

          val candles = Candles.retrieve( code, startDate, endDate, tickType )

          respondWithMediaType( `application/json` ) {
            complete( s"${ candles.toJson }" )
          }
        }
      }
    }
  }

}
