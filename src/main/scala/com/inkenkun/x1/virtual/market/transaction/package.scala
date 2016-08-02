package com.inkenkun.x1.virtual.market

package object transaction {

  abstract class Constant( val value: String )

  sealed abstract class How( override val value: String ) extends Constant( value ) {
    def isMarket: Boolean = this == How.market
    def isLimit : Boolean = this == How.limit
  }
  object How {
    case object market extends How( "market" )
    case object limit  extends How( "limit" )

    def apply( value: String ): How = value match {
      case market.value => market
      case limit.value  => limit
      case _ => market
    }
  }

  sealed abstract class Account( override val value: String ) extends Constant( value ) {
    def isCredit: Boolean = this == Account.credit
    def isCash  : Boolean = this == Account.cash
  }
  object Account {
    case object credit extends Account( "credit" )
    case object cash   extends Account( "cash" )

    def apply( value: String ): Account = value match {
      case credit.value => credit
      case cash.value   => cash
      case _  => cash
    }
  }

  sealed abstract class SoL( override val value: String ) extends Constant( value )
  object SoL {
    case object short extends SoL( "short" )
    case object long  extends SoL( "long" )

    def apply( value: String ): SoL = value match {
      case short.value => short
      case long.value  => long
      case _ => long
    }
  }

  sealed abstract class BoS( override val value: String ) extends Constant( value ) {
    def isBuy : Boolean = this == BoS.buy
    def isSell: Boolean = this == BoS.sell
  }
  object BoS {
    case object buy  extends BoS( "buy" )
    case object sell extends BoS( "sell" )

    def apply( value: String ): BoS = value match {
      case buy.value  => buy
      case sell.value => sell
      case _ => buy
    }
  }
}