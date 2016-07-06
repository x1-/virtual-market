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

  sealed abstract class Balance( val value: String )
  object Balance {
    case object credit extends How( "credit" )
    case object cash   extends How( "cash" )

    def apply( value: Option[String] ): How = value match {
      case Some( credit.value ) => credit
      case Some( cash.value )  => cash
      case _ => cash
    }
  }

}