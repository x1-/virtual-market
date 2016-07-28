package com.inkenkun.x1.virtual.market.stock

import scalikejdbc._
import com.inkenkun.x1.virtual.market.mysql.{Handler => MySQLHandler}

case class Stock(
  market: String,
  code  : String,
  name  : String
)

object Stock extends SQLSyntaxSupport[Stock] {
  def apply( cols: Seq[String] ): Stock =
    new Stock(
      market = cols.head,
      code   = cols( 1 ),
      name   = cols( 2 )
    )

  def apply( rs: WrappedResultSet ): Stock =
    new Stock(
      market = rs.string( "market" ),
      code   = rs.string( "code" ),
      name   = rs.string( "name" )
    )
}

object Stocks extends MySQLHandler {

  lazy val values: Vector[Stock] = {
    sql"""
      select * from
        market_stocks
      order by
        market
       ,code
    """.map( rs => Stock( rs ) ).list.apply().toVector
  }
}
