package com.inkenkun.x1.virtual.market

import akka.actor.Actor
import spray.http.MediaTypes._
import spray.routing._
import spray.routing.RejectionHandler.Default

import com.inkenkun.x1.virtual.market.stock._
import com.inkenkun.x1.virtual.market.transaction._
import com.inkenkun.x1.virtual.market.user.{Accounts, Contract}

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
        respondWithMediaType( `text/html` ) {
          AccountsManager ! "load"
          complete(
            <html>
              <head></head>
              <body><p>Status: loading</p></body>
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
    path( "user" / "add" ) {
      get {
        parameters( 'id, 'name ) { ( id, name ) =>
          AccountsManager ! ( "add", id, name )
          respondWithMediaType( `application/json` ) {
            complete(
              <html>
                <head></head>
                <body><p>Status: adding</p></body>
              </html>
            )
          }
        }
      }
    } ~
    path( "user" / "reset" ) {
      get {
        parameters( 'id ) { id =>
          AccountsManager ! ( "reset", id )
          respondWithMediaType( `application/json` ) {
            complete(
              <html>
                <head></head>
                <body><p>Status: resetting</p></body>
              </html>
            )
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
        parameters(
           'id, 'code, 'account ? "cash", 'sol ? "long", 'how ? "market", 'price.as[Double] ? 0d, 'number.as[Long] ? 100, 'expiration.? ) {
          ( id, code, account, sol, how, price, number, expiration ) =>
            val now      = marketNow

            val contract = Contract(
              userId     = id,
              code       = code,
              price      = price,
              volume     = number,
              account    = Account( account ),
              sol        = SoL( sol ),
              how        = How( how ),
              bos        = BoS.buy,
              expiration = expiration map timestampFormat.parseDateTime _ getOrElse now
            )
            val messages = contract.validate

            if ( messages.isEmpty ) {
              TransactionManager ! contract
            }

            respondWithMediaType( `application/json` ) {
              complete(
                s"""{"message":${messages.toJson},"jobId":"${contract.jobId}"}"""
              )
            }
        }
      }
    } ~
    path( "sell" ) {
      get {
        parameters(
           'id, 'code, 'account ? "cash", 'sol ? "long", 'how ? "market", 'price.as[Double] ? 0d, 'number.as[Long] ? 100, 'expiration.? ) {
          ( id, code, account, sol, how, price, number, expiration ) =>
            val now      = marketNow

            val contract = Contract(
              userId     = id,
              code       = code,
              price      = price,
              volume     = number,
              account    = Account( account ),
              sol        = SoL( sol ),
              how        = How( how ),
              bos        = BoS.sell,
              expiration = expiration map timestampFormat.parseDateTime _ getOrElse now
            )
            val messages = contract.validate

            if ( messages.isEmpty ) {
              TransactionManager ! contract
            }

            respondWithMediaType( `application/json` ) {
              complete(
                s"""{"message":${messages.toJson},"jobId":"${contract.jobId}"}"""
              )
            }
        }
      }
    } ~
    path( "price" ) {
      get {
        parameters( 'code, 'start.?, 'end.?, 'tick.? ) { ( code, start, end, tick ) =>
          val now       = marketNow
          val tickType  = Tick( tick )
          val tickMin   = Tick.tick2minutes( tickType )
          val startDate = start map timestampFormat.parseDateTime _ getOrElse now.minusMinutes( tickMin )
          val endDate   = end map timestampFormat.parseDateTime _ getOrElse now

          val candles = Candles.searchByTick( code, startDate, endDate, tickType )

          respondWithMediaType( `application/json` ) {
            complete( s"${ candles.toJson }" )
          }
        }
      }
    }
  }

}
