package com.inkenkun.x1.virtual.market.user

import org.joda.time.DateTime
import scalikejdbc.{SQLSyntaxSupport, WrappedResultSet}

import com.inkenkun.x1.virtual.market.transaction.SoL

case class Holding (
  userId : String,
  time   : DateTime,
  market : String,
  code   : String,
  price  : BigDecimal,
  volume : Long,
  soL    : SoL,
  id     : Option[Int] = None
)
object Holding extends SQLSyntaxSupport[Holding] {
  def apply( rs: WrappedResultSet ): Holding =
    new Holding(
      id     = rs.intOpt( "id" ),
      userId = rs.string( "user_id" ),
      time   = rs.jodaDateTime( "time" ),
      market = rs.string( "market" ),
      code   = rs.string( "code" ),
      price  = rs.bigDecimal( "price" ),
      volume = rs.long( "volume" ),
      soL    = SoL( rs.string( "short_or_long" ) )
    )
}

object Holdings {}
