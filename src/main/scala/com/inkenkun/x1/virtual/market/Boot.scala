package com.inkenkun.x1.virtual.market

import akka.actor.Props
import akka.io.IO
import akka.pattern.ask
import spray.can.Http

object Boot extends App {

  import implicits._

  // create and start our service actor
  val service = system.actorOf( Props[ServiceActor], "service" )

  // start a new HTTP server on port 8080 with our service actor as the handler
  IO( Http ) ? Http.Bind( service, interface = config.getString( "http.host" ), port = config.getInt( "http.port" ) )
}