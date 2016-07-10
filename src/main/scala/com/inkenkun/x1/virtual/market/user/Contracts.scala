package com.inkenkun.x1.virtual.market.user

import scala.collection.mutable.ListBuffer

import org.joda.time.DateTime

import com.inkenkun.x1.virtual.market.stock.Candles
import com.inkenkun.x1.virtual.market.transaction.{Account => AccType, BoS, How, SoL}

case class Contract (
  userId     : String,
  code       : String,
  account    : AccType,
  sol        : SoL,
  how        : How,
  price      : Double,
  number     : Int,
  expiration : DateTime,
  bos        : BoS,
  status     : Contracts.Status = Contracts.Status.notYet
) {
  import com.inkenkun.x1.virtual.market._

  lazy val startTime = marketNow

  lazy val user  = Accounts.retrieve( userId )
  lazy val stock = Candles.latest( code, startTime.plusMinutes( 3 ) )
  lazy val unit  = if ( how.isMarket ) stock.flatMap( s => Some( s.high ) ).getOrElse( BigDecimal(0) ) else BigDecimal( price )

  def validate: List[String] = {

    val errors = ListBuffer.empty[String]

    if ( user.userId.isEmpty ) {
      errors += s"アカウントIDが正しくありません。指定されたアカウントID: $userId"
    }
    if ( stock.isEmpty ) {
      errors += s"銘柄コードが正しくありません。指定された銘柄コード: $code, http://host/stocks/available で利用可能な銘柄を確認できます。"
    }
    if ( number % 100 != 0 ) {
      errors += "単元は100株単位で指定してください。"
    }
    if ( expiration.isBefore( startTime ) ) {
      errors += s"有効期限が過ぎています。 現在時刻:${ startTime.toString( timestampFormat ) }"
    }
    errors.toList
  }

  def validateAtBuying: List[String] = {

    val errors = ListBuffer.empty[String]

    if ( sol == SoL.short && account == AccType.cash ) {
      errors += "空売りの場合は信用取引を指定してください。"
    }
    if ( account == AccType.cash ) {
      user.availableCash.flatMap { c =>
        if ( unit * number > c )
          Some( s"口座残高が不足しています。現物取引可能残高: ${user.availableCash}, 単元価格: $unit" )
        else
          None
      } match {
        case Some( x ) => errors += x
        case None => _
      }
    }
    if ( account == AccType.credit ) {
      user.availableCredit.flatMap { c =>
        if ( unit * number > c )
          Some( s"口座残高が不足しています。信用取引可能残高: ${user.availableCredit}, 単元価格: $unit" )
        else
          None
      } match {
        case Some( x ) => errors += x
        case None => _
      }
    }
    validate ++ errors.toList
  }

  def validateAtSelling: List[String] = {

    val errors = ListBuffer.empty[String]

    if ( sol == SoL.short && account == AccType.cash ) {
      errors += "空売りの買い戻し場合は信用取引を指定してください。"
    }
    validate ++ errors.toList
  }
}

object Contracts {

  sealed abstract class Status( val value: String ) {
    def isNotYet     : Boolean = this == Status.notYet
    def isDone       : Boolean = this == Status.done
    def isImpossible : Boolean = this == Status.impossible
  }
  object Status {
    case object notYet     extends Status( "notyet" )
    case object done       extends Status( "done" )
    case object impossible extends Status( "impossible" )

    def apply( value: String ): Status = value match {
      case notYet.value     => notYet
      case done.value       => done
      case impossible.value => impossible
      case _ => notYet
    }
  }

}
