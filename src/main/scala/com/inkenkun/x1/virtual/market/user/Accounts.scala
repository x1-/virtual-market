package com.inkenkun.x1.virtual.market.user

import java.util.Date
import scala.collection.mutable.Map

import akka.actor.{Actor, ActorLogging}

import com.inkenkun.x1.virtual.market.redis


case class Account(
  userId          : String = "",
  userName        : String = "",
  balance         : Option[BigDecimal] = None,
  availableCash   : Option[BigDecimal] = None,
  availableCredit : Option[BigDecimal] = None,
  holdings        : List[Holding] = List.empty[Holding]
)

case class Holding (
  time   : Date,
  market : String,
  code   : String,
  price  : BigDecimal,
  volume : Int
)

object Accounts{

  import com.inkenkun.x1.virtual.market.implicits._

  type userId = String

  val users = Map.empty[userId, Account]

  lazy val initialUsers: List[Account] = {
    val maybeUsers = redis.Handler.get( "users" )
    maybeUsers map( _.parseAs[List[Account]] ) getOrElse List.empty[Account]
  }

  def retrieve( id: userId ): Account = users.getOrElse( id, Account() )

  private def load(): Unit = {
    initialUsers.map { acc =>
      users += ( acc.userId -> acc )
    }
  }

  class ManagerActor extends Actor with ActorLogging {
    def receive = {
      case "load" =>
        if ( users.nonEmpty ) {
          load()
        }
    }
  }
}
