package com.inkenkun.x1.virtual

import akka.actor.Props
import com.typesafe.config.ConfigFactory
import org.joda.time.{DateTime, Hours}
import org.joda.time.format.DateTimeFormat
import scalikejdbc._

import com.inkenkun.x1.virtual.market.stock.Candles
import com.inkenkun.x1.virtual.market.user.Accounts

package object market {

  import implicits._

  type market = String
  type code   = String
  type tick   = String
  type userId = String

  val config = ConfigFactory.load.getConfig( "market" )

  val startMills  = System.currentTimeMillis()

  val dateFormat         = DateTimeFormat.forPattern( "yyyy-MM-dd" )
  val timestampFormat    = DateTimeFormat.forPattern( "yyyy-MM-dd HH:mm:ss" )
  val baseTime           = timestampFormat.parseDateTime( config.getString( "start-time" ) )

  val CandleFetcher      = system.actorOf( Props[Candles.FetchActor], "CandlesFetcher" )
  val AccountsManager    = system.actorOf( Props[Accounts.ManagerActor], "AccountsManager" )
  val TransactionManager = system.actorOf( Props[transaction.Manager.JobActor], "TransactionManager" )
  val LogManager         = system.actorOf( Props[Log.ManagerActor], "LogManage" )

  implicit val marketStart = new DateTime( startMills )

  GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
    enabled = false,
    logLevel = 'DEBUG,
    singleLineMode = true,
    warningEnabled = true,
    warningThresholdMillis = 1000L,
    warningLogLevel = 'WARN
  )

  def marketTime( baseTime: DateTime, currentMills: Long )( implicit marketStart: DateTime ): DateTime = {
    val now      = new DateTime( currentMills )
    val interval = Hours.hoursBetween( marketStart, now ).getHours
    val diff     = interval / 6
    val mod      = interval % 6

    val newTime = baseTime.plusDays( diff )
    newTime
      .withHourOfDay( mod + 9 )
      .withMinuteOfHour( now.getMinuteOfHour )
      .withSecondOfMinute( now.getSecondOfMinute )
  }

  def marketNow: DateTime = marketTime( baseTime, baseTime.plus( System.currentTimeMillis - startMills ).getMillis )

  def adjustDay( targetDate: DateTime )( implicit marketStart: DateTime ): DateTime = {

    val newTime = targetDate

    targetDate match {
      case x if targetDate.getHourOfDay > 14 =>
        newTime
          .withHourOfDay( 14 )
          .withMinuteOfHour( 59 )
          .withSecondOfMinute( 59 )
      case x if targetDate.getHourOfDay < 9 =>
        newTime
          .minusDays( 1 )
          .withHourOfDay( 14 )
          .withMinuteOfHour( 59 )
          .withSecondOfMinute( 59 )
      case _ => newTime
    }
  }

}
