package com.inkenkun.x1.virtual.market.stock

import java.util.Date

import org.specs2.mutable.Specification

class CandlesSpec extends Specification {

  import com.inkenkun.x1.virtual.market.implicits._

  "Candle" should {
    "serialized with DateTime" in {
      val candles = Vector(
        Candle(
          new Date,
          "TYO",
          "1332",
          BigDecimal( 567 ),
          BigDecimal( 570 ),
          BigDecimal( 552 ),
          BigDecimal( 567 ),
          123400L
        ),
        Candle(
          new Date,
          "TYO",
          "1332",
          BigDecimal( 567 ),
          BigDecimal( 575 ),
          BigDecimal( 567 ),
          BigDecimal( 575 ),
          223400L
        )
      )
      val json = candles.toJson
      println( json )
      json must not beEmpty
    }
  }
}
