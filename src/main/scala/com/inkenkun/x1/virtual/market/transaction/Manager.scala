package com.inkenkun.x1.virtual.market.transaction

import scala.util.Random

import akka.actor.{Actor, ActorLogging}

import com.inkenkun.x1.virtual.market.user.Contract


object Manager {

  def jobId = Random.nextInt( 99999999 ).toString

  def buy( contract: Contract ) = {

  }
  class JobActor extends Actor with ActorLogging {
    def receive = {
      case ( "buy", contract: Contract ) =>
        buy( contract )
    }
  }
}