package com.inkenkun.x1.virtual.market.user

import java.util.Date
import scala.collection.mutable

import akka.actor.{Actor, ActorLogging}

import com.inkenkun.x1.virtual.market.redis

case class Account(
  userId          : String = "",
  userName        : String = "",
  availableCash   : Option[BigDecimal] = None,
  availableCredit : Option[BigDecimal] = None,
  holdings        : List[Holding] = List.empty[Holding],
  contracted      : List[Contract] = List.empty[Contract],
  notContracted   : List[Contract] = List.empty[Contract]
)

case class Holding (
  time   : Date,
  market : String,
  code   : String,
  price  : BigDecimal,
  volume : Int
)

object Accounts{

  import com.inkenkun.x1.virtual.market.userId
  import com.inkenkun.x1.virtual.market.implicits._

  val users = mutable.Map.empty[userId, Account]

  lazy val initialUsers: List[Account] = {
    val maybeUsers = redis.Handler.get( "users" )
    maybeUsers map( _.parseAs[List[Account]] ) getOrElse List.empty[Account]
  }

  def retrieve( id: userId ): Account = users.getOrElse( id, Account() )

  private def updateNotContracted( contract: Contract ): Unit = synchronized {
    for {
      user   <- users.get( contract.userId )
      newUser = user.copy( notContracted = notContracted + contract )
    } yield {
      users.update( userId, newUser )
    }
  }
  private def update( user: Account ): Unit = synchronized {
    users.update( user.userId, user )
  }

  private def load(): Unit = synchronized {
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

      case ( "yet", contract: Contract ) =>
        updateNotContracted( contract )

      case ( "update", user: Account ) =>
        update( user )

    }
  }
}
