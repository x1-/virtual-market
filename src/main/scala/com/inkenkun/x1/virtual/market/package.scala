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

  val config             = ConfigFactory.load.getConfig( "market" )

  val startMills         = System.currentTimeMillis
  
  val dateFormat         = DateTimeFormat.forPattern( "yyyy-MM-dd" )
  val timestampFormat    = DateTimeFormat.forPattern( "yyyy-MM-dd HH:mm:ss" )

  val CandleFetcher      = system.actorOf( Props[Candles.FetchActor], "CandlesFetcher" )
  val AccountsManager    = system.actorOf( Props[Accounts.ManagerActor], "AccountsManager" )
  val TransactionManager = system.actorOf( Props[transaction.Manager.JobActor], "TransactionManager" )
  val LogManager         = system.actorOf( Props[Log.ManagerActor], "LogManage" )

  implicit val baseTime  = timestampFormat.parseDateTime( config.getString( "start-time" ) )

  GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
    enabled = false,
    logLevel = 'DEBUG,
    singleLineMode = true,
    warningEnabled = true,
    warningThresholdMillis = 1000L,
    warningLogLevel = 'WARN
  )

  def marketTime( currentMills: Long, startMills: Long )( implicit baseTime: DateTime ): DateTime = {
    val marketStart = new DateTime( startMills )
    val now         = new DateTime( currentMills )
    val after       = baseTime.plus( currentMills - startMills )
    val interval    = Hours.hoursBetween( marketStart, now ).getHours
    val diff        = interval / 6
    val mod         = interval % 6
    
    val newTime = baseTime.plusDays( diff )
    newTime
      .withHourOfDay( mod )
      .withMinuteOfHour( after.getMinuteOfHour )
      .withSecondOfMinute( after.getSecondOfMinute )
  }

  def marketNow: DateTime = marketTime( System.currentTimeMillis, startMills )

}
