package com.inkenkun.x1.virtual.market

import org.joda.time.format.DateTimeFormat
import org.specs2.mutable.Specification

class PackageSpec extends Specification {

  "market.package.marketTime" should {

    val timestampFormat = DateTimeFormat.forPattern( "yyyy-MM-dd HH:mm:ss" )
    val baseTime        = timestampFormat.parseDateTime( "2015-04-01 09:00:00" )
    implicit val marketStart = timestampFormat.parseDateTime( "2016-01-01 00:00:00" )

    "return 2015-04-01 14:59:59 when currentMills is 2016-01-01 5:59:59" in {
      val currentMills = timestampFormat.parseDateTime( "2016-01-01 5:59:59" ).getMillis
      val newTime       = marketTime( baseTime, currentMills )

      newTime.getYear           must_== 2015
      newTime.getMonthOfYear    must_== 4
      newTime.getDayOfMonth     must_== 1
      newTime.getHourOfDay      must_== 14
      newTime.getMinuteOfHour   must_== 59
      newTime.getSecondOfMinute must_== 59
    }
    "return 2015-04-02 14:59:59 when time is 2016-01-01 11:59:59" in {
      val currentMills = timestampFormat.parseDateTime( "2016-01-01 11:59:59" ).getMillis
      val newTime      = marketTime( baseTime, currentMills )

      newTime.getYear           must_== 2015
      newTime.getMonthOfYear    must_== 4
      newTime.getDayOfMonth     must_== 2
      newTime.getHourOfDay      must_== 14
      newTime.getMinuteOfHour   must_== 59
      newTime.getSecondOfMinute must_== 59
    }
  }
}
