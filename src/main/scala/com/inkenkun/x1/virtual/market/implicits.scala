package com.inkenkun.x1.virtual.market

import java.text.SimpleDateFormat
import java.util.Date

import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.util.Timeout
import org.json4s.{CustomSerializer, DateFormat, DefaultFormats, JField, JObject, JString}
import org.json4s.jackson.{JsonMethods, Serialization}
import com.inkenkun.x1.virtual.market.stock.{Candle, Stock}
import com.inkenkun.x1.virtual.market.transaction.{Account => AccType, BoS, How, SoL}
import com.inkenkun.x1.virtual.market.user.{Account, Contract}
import com.inkenkun.x1.virtual.market.user.Contracts.Status

object implicits {

  implicit val system = ActorSystem( "virtual-market" )
  implicit val timeout = Timeout( 5.seconds )

  implicit val formats = new DefaultFormats {

    val timestampFormat = new SimpleDateFormat( "yyyy-MM-dd HH:mm:ss" )

    override val dateFormat: DateFormat = new DateFormat {
      override def parse( s: String ): Option[Date] = Some( timestampFormat.parse( s ) )
      override def format( d: Date ): String = timestampFormat.format( d )
    }
  } + new AccTypeSerializer + new BoSSerializer + new HowSerializer + new SoLSerializer + new StatusSerializer
  

  implicit class JsonSerializable[T](val a: T) extends AnyVal {
    def toJson(implicit serializer: JsonSerializer[T]): String =
      serializer.toJson(a)
  }

  implicit class JsonDeserializable(val json: String) extends AnyVal {
    def parseAs[T](implicit serializer: JsonSerializer[T]): T =
      serializer.fromJson(json)
  }

  trait JsonSerializer[T] {
    def toJson(a: T): String
    def fromJson(json: String): T
  }

  implicit val AccountSerializer = new JsonSerializer[Account] {
    def toJson(a: Account): String = Serialization.write(a)
    def fromJson(json: String): Account = JsonMethods.parse(json).extract[Account]
  }
  implicit val AccountsSerializer = new JsonSerializer[List[Account]] {
    def toJson(a: List[Account]): String = Serialization.write(a)
    def fromJson(json: String): List[Account] = JsonMethods.parse(json).extract[List[Account]]
  }
  implicit val ContractSerializer = new JsonSerializer[Contract] {
    def toJson(a: Contract): String = Serialization.write(a)
    def fromJson(json: String): Contract = JsonMethods.parse(json).extract[Contract]
  }
  implicit val ContractsSerializer = new JsonSerializer[List[Contract]] {
    def toJson(a: List[Contract]): String = Serialization.write(a)
    def fromJson(json: String): List[Contract] = JsonMethods.parse(json).extract[List[Contract]]
  }

  implicit val CandlesSerializer = new JsonSerializer[Vector[Candle]] {
    def toJson(a: Vector[Candle]): String = Serialization.write(a)
    def fromJson(json: String): Vector[Candle] = JsonMethods.parse(json).extract[Vector[Candle]]
  }

  implicit val StockSerializer = new JsonSerializer[Vector[Stock]] {
    def toJson(a: Vector[Stock]): String = Serialization.write(a)
    def fromJson(json: String): Vector[Stock] = JsonMethods.parse(json).extract[Vector[Stock]]
  }

  implicit val ListStringSerializer = new JsonSerializer[List[String]] {
    def toJson(a: List[String]): String = Serialization.write(a)
    def fromJson(json: String): List[String] = JsonMethods.parse(json).extract[List[String]]
  }


  class AccTypeSerializer extends CustomSerializer[AccType]( format => ({
      case JString( v ) =>
        AccType( v )
    }, {
      case x: AccType =>
        JString( x.value )
    }) )

  class BoSSerializer extends CustomSerializer[BoS]( format => ({
      case JString( v ) =>
        BoS( v )
    }, {
      case x: BoS =>
        JString( x.value )
    }) )

  class HowSerializer extends CustomSerializer[How]( format => ({
      case JString( v ) =>
        How( v )
    }, {
      case x: How =>
        JString( x.value )
    }) )

  class SoLSerializer extends CustomSerializer[SoL]( format => ({
      case JString( v ) =>
        SoL( v )
    }, {
      case x: SoL =>
        JString( x.value )
    }) )

  class StatusSerializer extends CustomSerializer[Status]( format => ({
      case JString( v ) =>
        Status( v )
    }, {
      case x: Status =>
        JString( x.value )
    }) )
}
