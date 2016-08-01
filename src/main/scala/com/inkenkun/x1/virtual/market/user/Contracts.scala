package com.inkenkun.x1.virtual.market.user

import scala.collection.mutable.ListBuffer
import scala.util.Random

import org.joda.time.DateTime
import scalikejdbc.{SQLSyntaxSupport, WrappedResultSet}

import com.inkenkun.x1.virtual.market.stock.Candles
import com.inkenkun.x1.virtual.market.transaction.{Account => AccType, BoS, How, SoL}

case class Contract (
  userId     : String,
  market     : String = "TYO",
  code       : String,
  price      : BigDecimal,
  volume     : Long,
  account    : AccType,
  sol        : SoL,
  how        : How,
  bos        : BoS,
  expiration : DateTime,
  status     : Contracts.Status = Contracts.Status.notYet,
  id         : Option[Int] = None
) {
  import com.inkenkun.x1.virtual.market._

  lazy val startTime = marketNow

  lazy val jobId = Random.nextInt( 99999999 ).toString
  lazy val user  = Accounts.retrieve( userId )
  lazy val stock = Candles.latest( code, startTime.plusMinutes( 3 ).toDate )
  lazy val unit  = if ( how.isMarket ) stock.map( _.high ).getOrElse( BigDecimal(0) ) else price

  def validate: List[String] = {

    val errors = ListBuffer.empty[String]

    if ( user.userId.isEmpty ) {
      errors += s"アカウントIDが正しくありません。指定されたアカウントID: $userId"
    }
    if ( stock.isEmpty ) {
      errors += s"銘柄コードが正しくありません。指定された銘柄コード: $code, http://host/stocks/available で利用可能な銘柄を確認できます。"
    }
    if ( volume % 100 != 0 ) {
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
    if ( account == AccType.cash && unit * volume > user.availableCash ) {
      errors += s"口座残高が不足しています。現物取引可能残高: ${user.availableCash}, 単元価格: $unit"
    }
    if ( account == AccType.credit && unit * volume > user.availableCredit ) {
      errors += s"口座残高が不足しています。信用取引可能残高: ${user.availableCredit}, 単元価格: $unit"
    }
    validate ++ errors.toList
  }

  def validateAtSelling: List[String] = {

    val errors = ListBuffer.empty[String]

    if ( sol == SoL.short && account == AccType.cash ) {
      errors += "空売りの買い戻し場合は信用取引を指定してください。"
    }
    val holding = user.holdings.find( p => p.market == market && p.code == code )
    holding match {
      case Some( x ) =>
        if ( x.volume < volume ) {
          errors += s"指定された銘柄の保持株数が売却数より少ないです。保持数: ${x.volume}, 売却数: $volume"
        }
      case None =>
        errors += s"指定された銘柄コード: $code を保持していないため売却もしくは買い戻しを行うことができません。"
    }
    validate ++ errors.toList
  }
}

object Contract extends SQLSyntaxSupport[Contract] {
  def apply( rs: WrappedResultSet ): Contract =
    new Contract(
      userId     = rs.string( "user_id" ),
      market     = rs.string( "market" ),
      code       = rs.string( "code" ),
      price      = rs.bigDecimal( "price" ),
      volume     = rs.long( "volume" ),
      account    = AccType( rs.string( "account" ) ),
      sol        = SoL( rs.string( "short_or_long" ) ),
      how        = How( rs.string( "how" ) ),
      bos        = BoS( rs.string( "buy_or_sell" ) ),
      expiration = rs.jodaDateTime( "expiration" ),
      status     = Contracts.Status( rs.string( "status" ) ),
      id         = rs.intOpt( "id" )
    )
}

object Contracts {

  import com.inkenkun.x1.virtual.market.userId

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

  def retrieveNotYets( userId: userId ) = UserDao.retrieve( userId ).notContracted

  def retrieveDones( userId: userId )   = UserDao.retrieve( userId ).contracted
}