package com.inkenkun.x1.virtual.market.stock

import java.util.Date

import akka.actor.{Actor, ActorLogging}
import scalikejdbc._
import org.joda.time.DateTime

import com.inkenkun.x1.virtual.market.bigquery.Handler
import com.inkenkun.x1.virtual.market.mysql.{Handler => MySQLHandler}

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
  volume : Long
)
object Candle extends SQLSyntaxSupport[Candle] {
  def apply ( cols: Seq[String] ): Candle =
    new Candle (
      time   = new Date( BigDecimal( cols.head ).toLong * 1000 ),
      market = cols( 1 ),
      code   = cols( 2 ),
      open   = BigDecimal( cols( 3 ) ),
      high   = BigDecimal( cols( 4 ) ),
      low    = BigDecimal( cols( 5 ) ),
      close  = BigDecimal( cols( 6 ) ),
      volume = cols( 7 ).toLong
    )
  
  def apply( rs: WrappedResultSet ): Candle =
    new Candle(
      time   = rs.date( "time" ),
      market = rs.string( "market" ),
      code   = rs.string( "code" ),
      open   = rs.bigDecimal( "open" ),
      high   = rs.bigDecimal( "high" ),
      low    = rs.bigDecimal( "low" ),
      close  = rs.bigDecimal( "close" ),
      volume = rs.long( "volume" )
    )
}

object Candles extends MySQLHandler {

  import com.inkenkun.x1.virtual.market._

  val projectId = config.getString( "bigquery.project_id" )

  val sql2 =
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
    val rs = Handler.executeQuery( sql2.format( tableName, code, startDate, endDate ), projectId )
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

    val total = Stocks.values.length

    candles1m = Stocks.values.foldLeft( Map.empty[code, Vector[Candle]] ) { ( dict, stock ) =>
      val candles = sql"""
        select * from
          candle_1min
        where
          code = ${stock.code}
          and time >= ${start.toString( timestampFormat )}
          and time >= ${end.toString( timestampFormat )}
        order by
          time asc
      """.map( rs => Candle( rs ) ).list.apply()

      if ( dict.size % 100 == 0 ) {
        println( s"${dict.size} / $total stocks fetched." )
      }
      dict + ( stock.code -> candles.toVector )
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


  def latest( code: String, time: Date ): Option[Candle] = {
    val m1 = latestByTick( code, Tick.m1, time )
    val m5 = latestByTick( code, Tick.m5, time )
    val d1 = latestByTick( code, Tick.d1, time )

    latestByDefined( latestByDefined( m1, m5 ), d1 )
  }
  private def latestByTick( code: String, tick: Tick, now: Date ): Option[Candle] = {
    val ticks = tick match {
      case Tick.m1 => candles1m
      case Tick.m5 => candles5m
      case Tick.d1 => candles1d
    }
    ticks.get( code ) map ( cv => cv.filter( _.time.before( now ) ).maxBy( _.time.getTime ) )
  }
  private def latestByDefined( c1: Option[Candle], c2: Option[Candle] ): Option[Candle] = ( c1, c2 ) match {
    case ( Some(x), Some(y) ) => Some( if ( x.time.after( y.time ) ) x else y )
    case ( Some(x), None )    => Some( x )
    case ( None, Some(y) )    => Some( y )
    case ( None, None )       => None
  }

  def searchByTick( code: String, startDate: DateTime, endDate: DateTime, tick: Tick ): Vector[Candle] = {

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

  def isReached( code: String, price: BigDecimal, startDate: DateTime, endDate: DateTime ): Boolean = {
    val all: Vector[Candle] =
      searchByTick( code, startDate, endDate, Tick.m1 ) ++
      searchByTick( code, startDate, endDate, Tick.m5 ) ++
      searchByTick( code, startDate, endDate, Tick.d1 ) ++ latest( code, startDate.toDate ).map( List(_) ).getOrElse( Nil )
    val highest = all.maxBy( _.high ).high
    val lowest  = all.minBy( _.low ).low

    lowest <= price && price <= highest
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
