package com.inkenkun.x1.virtual.market.stock

import java.util.Date

import org.joda.time.DateTime
import akka.actor.{Actor, ActorLogging}
import com.inkenkun.x1.virtual.market.bigquery.Handler

sealed abstract class Tick( val value: String )
object Tick {
  case object m1 extends Tick( "1m" )
  case object m5 extends Tick( "5m" )
  case object d1 extends Tick( "1d" )

  def apply( value: Option[String] ): Tick = value match {
    case Some( m1.value ) => m1
    case Some( m5.value ) => m5
    case Some( d1.value ) => d1
    case _ => m1
  }
  def tick2minutes( tick: Tick ): Int = tick match {
    case `m1` => 1
    case `m5` => 5
    case `d1` => 24 * 60
  }
}

case class Candle (
  time   : Date,
  market : String,
  code   : String,
  open   : BigDecimal,
  high   : BigDecimal,
  low    : BigDecimal,
  close  : BigDecimal,
  volume : Int
)
object Candle {
  def apply ( cols: Seq[String] ): Candle =
    new Candle (
      time   = new Date( BigDecimal( cols.head ).toLong * 1000 ),
      market = cols( 1 ),
      code   = cols( 2 ),
      open   = BigDecimal( cols( 3 ) ),
      high   = BigDecimal( cols( 4 ) ),
      low    = BigDecimal( cols( 5 ) ),
      close  = BigDecimal( cols( 6 ) ),
      volume = cols( 7 ).toInt
    )
}

object Candles {

  import com.inkenkun.x1.virtual.market._

  val projectId = config.getString( "bigquery.project_id" )

  val sql =
    s"""
      |select
      |    time
      |   ,market
      |   ,code
      |   ,open
      |   ,high
      |   ,low
      |   ,close
      |   ,volume
      | from
      |   [stocks.%s]
      | where
      |   code %s
      |   and time >= timestamp( '%s' )
      |   and time <= timestamp( '%s' )
      |""".stripMargin

  val executeFunc: ( String, String ) => ( String, String ) => Vector[Candle] = ( startDate, endDate ) => ( tableName, code ) => {
    val rs = Handler.executeQuery( sql.format( tableName, code, startDate, endDate ), projectId )
    rs.toVector.map{ row => Candle ( row ) } sortBy( _.time )
  }

  var candles1m: Map[code, Vector[Candle]] = Map.empty[code, Vector[Candle]]
  var candles5m: Map[code, Vector[Candle]] = Map.empty[code, Vector[Candle]]
  var candles1d: Map[code, Vector[Candle]] = Map.empty[code, Vector[Candle]]

  var dictionary: Map[code, Map[tick, Vector[Candle]]] = Map.empty[code, Map[tick, Vector[Candle]]]
  var status = "notyet"

  def fetch1m( start: DateTime, end: DateTime ): Unit = synchronized {
    status = "fetch1m start"
    println( status )

    val execute = executeFunc( start.toString( timestampFormat ), end.toString( timestampFormat ) )

    val total = Stocks.values.length

    candles1m = Stocks.values.foldLeft( Map.empty[code, Vector[Candle]] ) { ( dict, stock ) =>
      if ( dict.size % 100 == 0 ) {
        println( s"${dict.size} / $total stocks fetched." )
      }
      dict + ( stock.code -> execute( "1min_candle", s"= '${stock.code}'" ) )
    }
    status = "fetch1m fetched"
    println( status )
  }

  def fetch5m( start: DateTime, end: DateTime ): Unit = synchronized {
    status = "fetch5m start"
    println( status )

    val execute = executeFunc( start.toString( timestampFormat ), end.toString( timestampFormat ) )

    val total  = Stocks.values.length
    val offset = ( total / 100 ) + ( if ( total % 100 == 0 ) 0 else 1 )

    val dumps = for ( i <- 0 to 100 ) yield {
      Stocks.values.slice( i * offset, scala.math.min( (i+1) * offset, total ) )
    }

    dumps.foreach { stocks =>
      val whereCondition = "in ('" + stocks.map( _.code ).mkString( "', '" ) + "')"
      val cs = execute( "5min_candle", whereCondition )
      candles5m = stocks.foldLeft( candles5m ) { ( dict, stock ) =>
        if ( dict.size % 100 == 0 ) {
          println( s"${dict.size} / $total stocks fetched." )
        }
        dict + ( stock.code -> cs.filter( _.code == stock.code ) )
      }
    }

    status = "fetch5m fetched"
    println( status )
  }


  def fetch1d( start: DateTime, end: DateTime ): Unit = synchronized {
    status = "fetch1d start"
    println( status )

    val execute        = executeFunc( start.toString( timestampFormat ), end.toString( timestampFormat ) )
    val whereCondition = "is not null"
    val cs = execute( "daily_candle", whereCondition )

    val total  = Stocks.values.length

    candles1d = Stocks.values.foldLeft( Map.empty[code, Vector[Candle]] ){ ( dict, stock ) =>
      if ( ( dict.size + 1 ) % 100 == 0 ) {
        println( s"${dict.size + 1} / $total stocks fetched." )
      }
      dict + ( stock.code -> cs.filter( _.code == stock.code ) )
    }

    status = "fetch1d fetched"
    println( status )
  }


  def retrieve( code: String, startDate: DateTime, endDate: DateTime, tick: Tick ): Vector[Candle] = {

    val start = startDate.getMillis
    val end   = endDate.getMillis

    val ticks = tick match {
      case Tick.m1 => candles1m
      case Tick.m5 => candles5m
      case Tick.d1 => candles1d
    }

    ticks
      .getOrElse( code, Vector.empty[Candle] )
      .collect{
        case x if x.time.getTime >= start && x.time.getTime <= end => x
      }
      .sortBy( _.time )
  }


  class FetchActor extends Actor with ActorLogging {
    def receive = {
      case ( Tick.m1, start: DateTime, end: DateTime ) =>
        Candles.fetch1m( start, end )
      case ( Tick.m5, start: DateTime, end: DateTime ) =>
        Candles.fetch5m( start, end )
      case ( Tick.d1, start: DateTime, end: DateTime ) =>
        Candles.fetch1d( start, end )
    }
  }
}