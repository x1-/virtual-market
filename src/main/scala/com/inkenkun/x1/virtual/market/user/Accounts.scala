package com.inkenkun.x1.virtual.market.user

import com.inkenkun.x1.virtual.market.redis

case class Account(
  userId          : String,
  userName        : String,
  balance         : Option[BigDecimal],
  marginRemaining : Option[BigDecimal]
)

object Accounts{

  import com.inkenkun.x1.virtual.market.implicits._

  lazy val initialUsers: List[Account] = {
    val maybeUsers = redis.Handler.get( "users" )
    maybeUsers map( _.parseAs[List[Account]] ) getOrElse List.empty[Account]
  }
}
