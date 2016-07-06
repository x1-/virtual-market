package com.inkenkun.x1.virtual.market

package object transaction {

  sealed abstract class How( val value: String )
  object How {
    case object market extends How( "market" )
    case object limit  extends How( "limit" )

    def apply( value: Option[String] ): How = value match {
      case Some( market.value ) => market
      case Some( limit.value )  => limit
      case _ => market
    }
  }

  sealed abstract class Account( val value: String )
  object Account {
    case object credit extends How( "credit" )
    case object cash   extends How( "cash" )

    def apply( value: Option[String] ): How = value match {
      case Some( credit.value ) => credit
      case Some( cash.value )  => cash
      case _ => cash
    }
  }

  sealed abstract class SoL( val value: String )
  object SoL {
    case object short extends How( "short" )
    case object long   extends How( "long" )

    def apply( value: Option[String] ): SoL = value match {
      case Some( short.value ) => short
      case Some( long.value )  => long
      case _ => long
    }
  }
}