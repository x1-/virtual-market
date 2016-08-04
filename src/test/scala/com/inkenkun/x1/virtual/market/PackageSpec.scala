package com.inkenkun.x1.virtual.market

import org.joda.time.format.DateTimeFormat
import org.specs2.mutable.Specification

class PackageSpec extends Specification {

  "market.package.marketTime" should {

    val timestampFormat = DateTimeFormat.forPattern( "yyyy-MM-dd HH:mm:ss" )
    val marketStart = timestampFormat.parseDateTime( "2016-01-01 03:00:10" )
    implicit val baseTime = timestampFormat.parseDateTime( "2015-04-01 00:00:00" )

    "return 2015-04-01 00:00:20 when currentMills is 2016-01-01 03:00:30" in {
      val currentMills = timestampFormat.parseDateTime( "2016-01-01 03:00:30" ).getMillis
      val startMills   = marketStart.getMillis
      val newTime      = marketTime( currentMills, startMills )

      newTime.getYear           must_== 2015
      newTime.getMonthOfYear    must_== 4
      newTime.getDayOfMonth     must_== 1
      newTime.getHourOfDay      must_== 0
      newTime.getMinuteOfHour   must_== 0
      newTime.getSecondOfMinute must_== 20
    }
    "return 2015-04-01 05:59:49 when currentMills is 2016-01-01 08:59:59" in {
      val currentMills = timestampFormat.parseDateTime( "2016-01-01 08:59:59" ).getMillis
      val startMills   = marketStart.getMillis
      val newTime      = marketTime( currentMills, startMills )

      newTime.getYear           must_== 2015
      newTime.getMonthOfYear    must_== 4
      newTime.getDayOfMonth     must_== 1
      newTime.getHourOfDay      must_== 5
      newTime.getMinuteOfHour   must_== 59
      newTime.getSecondOfMinute must_== 49
    }
    "return 2015-04-02 14:59:59 when currentMills is 2016-01-01 15:59:10" in {
      val currentMills = timestampFormat.parseDateTime( "2016-01-01 15:59:10" ).getMillis
      val startMills   = marketStart.getMillis
      val newTime      = marketTime( currentMills, startMills )

      newTime.getYear           must_== 2015
      newTime.getMonthOfYear    must_== 4
      newTime.getDayOfMonth     must_== 3
      newTime.getHourOfDay      must_== 0
      newTime.getMinuteOfHour   must_== 59
      newTime.getSecondOfMinute must_== 0
    }
  }
}
