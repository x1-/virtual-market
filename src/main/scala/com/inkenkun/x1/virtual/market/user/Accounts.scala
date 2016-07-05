package com.inkenkun.x1.virtual.market.user

import java.util.Date
import com.inkenkun.x1.virtual.market.redis

case class Account(
  userId          : String = "",
  userName        : String = "",
  balance         : Option[BigDecimal] = None,
  marginRemaining : Option[BigDecimal] = None,
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

  lazy val initialUsers: List[Account] = {
    val maybeUsers = redis.Handler.get( "users" )
    maybeUsers map( _.parseAs[List[Account]] ) getOrElse List.empty[Account]
  }

  def retrieve( id: String ): Account =
    initialUsers.find( _.userId == id ).getOrElse( Account() )

}
