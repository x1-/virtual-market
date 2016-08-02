package com.inkenkun.x1.virtual.market

import akka.actor.{Actor, ActorLogging}

case class Log(
  message: String,
  tag    : String = "",
  e      : Option[Throwable] = None
)
object Log {

  class ManagerActor extends Actor with ActorLogging {
    def receive = {
      case Log( message, tag, None ) =>
        log.info( s"#$tag# $message" )

      case Log( message, tag, Some(e) ) =>
        log.error( e, s"#$tag# $message" )
    }
  }

}
