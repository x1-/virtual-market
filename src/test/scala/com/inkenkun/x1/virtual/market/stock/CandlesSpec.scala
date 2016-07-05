package com.inkenkun.x1.virtual.market.stock

import org.specs2.mutable.Specification

class CandlesSpec extends Specification {

  import com.inkenkun.x1.virtual.market.implicits._

  "Candle" should {
    "serialized with DateTime" in {
      val candles = Vector(
        Candle( Vector(
          "1467465276",
          "TYO",
          "1332",
          "567",
          "570",
          "552",
          "567",
          "123400"
        ) ),
        Candle( Vector(
          "1467551676",
          "TYO",
          "1332",
          "567",
          "575",
          "567",
          "575",
          "223400"
        ) )
      )
      val json = candles.toJson
      println( json )
      json must not beEmpty
    }
  }
}
