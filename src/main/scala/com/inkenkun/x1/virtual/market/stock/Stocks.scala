package com.inkenkun.x1.virtual.market.stock

import com.inkenkun.x1.virtual.market.bigquery.Handler

case class Stock(
  market: String,
  code  : String,
  name  : String
)

object Stock {
  def apply( cols: Seq[String] ): Stock =
    new Stock(
      market = cols.head,
      code   = cols( 1 ),
      name   = cols( 2 )
    )
}

object Stocks {

  import com.inkenkun.x1.virtual.market._

  val projectId = config.getString( "bigquery.project_id" )

  val sql =
    s"""
      |select
      |   market
      |  ,code
      |  ,name
      |from
      |  [stocks.market_stocks]
      |order by
      |   market
      |  ,code
      |""".stripMargin


  lazy val values: Vector[Stock] = {
    val rs = Handler.executeQuery( sql, projectId )
    rs.toVector.map{ Stock( _ ) }
  }
}
