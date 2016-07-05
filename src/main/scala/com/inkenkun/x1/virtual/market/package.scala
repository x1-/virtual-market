package com.inkenkun.x1.virtual

import akka.actor.Props
import com.typesafe.config.ConfigFactory
import org.joda.time.{DateTime, Hours}
import org.joda.time.format.DateTimeFormat

import com.inkenkun.x1.virtual.market.stock.Candles

package object market {

  import implicits._

  type market = String
  type code   = String
  type tick   = String

  val config = ConfigFactory.load.getConfig( "market" )

  val startMills  = System.currentTimeMillis()
  val marketStart = new DateTime( startMills )

  val dateFormat      = DateTimeFormat.forPattern( "yyyy-MM-dd" )
  val timestampFormat = DateTimeFormat.forPattern( "yyyy-MM-dd HH:mm:ss" )
  val baseTime        = timestampFormat.parseDateTime( config.getString( "start-time" ) )

  val CandleFetcher = system.actorOf( Props[Candles.FetchActor], "CandlesFetcher" )

  def marketTime( baseTime: DateTime, currentMills: Long )( implicit marketStart: DateTime ): DateTime = {
    val now = new DateTime( currentMills )
    val interval = Hours.hoursBetween( marketStart, now ).getHours
    val diff = interval / 6
    val mod  = interval % 6

    val newTime = baseTime.plusDays( diff )
    newTime
      .withHourOfDay( mod + 9 )
      .withMinuteOfHour( now.getMinuteOfHour )
      .withSecondOfMinute( now.getSecondOfMinute )
  }
}
