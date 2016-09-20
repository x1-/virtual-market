package com.inkenkun.x1.virtual.market.user

import org.joda.time.format.DateTimeFormat
import org.specs2.mutable.Specification

import com.inkenkun.x1.virtual.market.stock.{Candle, Candles}

class AccountSpec extends Specification {
  
  import com.inkenkun.x1.virtual.market._
  import implicits._

  val timestampFormat = DateTimeFormat.forPattern( "yyyy-MM-dd HH:mm:ss" )

  val holding1 = Holding(
    userId = "000000",
    time   = marketNow,
    market = "TYO",
    code   = "1332",
    price  = BigDecimal(100),
    volume = 200,
    soL    = transaction.SoL.long
  )
  val holding2 = Holding(
    userId = "000000",
    time   = marketNow,
    market = "TYO",
    code   = "1333",
    price  = BigDecimal(200),
    volume = 200,
    soL    = transaction.SoL.short
  )

  "Account.calcAvailableCash" should {

    val contract = Contract (
      userId     = "111111",
      code       = "1332",
      account    = transaction.Account.cash,
      sol        = transaction.SoL.long,
      how        = transaction.How.limit,
      price      = 95d,
      volume     = 100,
      expiration = marketNow,
      bos        = transaction.BoS.buy,
      market     = "TYO"
    )

    val user = Account(
      userId     = "111111",
      userName        = "test",
      availableCash   = BigDecimal( 1000000 ),
      availableCredit = BigDecimal( 1000000 ),
      balance         = BigDecimal( 1000000 ),
      loan            = BigDecimal( 0 ),
      holdings        = List( holding1, holding2 ),
      contracted      = List.empty[Contract],
      notContracted   = List( contract.copy( status = Contracts.Status.notYet ) )
    )

    "return same cash when Buy and contract.status.done" in {
      user.calcAvailableCash( contract.copy( status = Contracts.Status.done ) ) must_== BigDecimal( 1000000 )
    }
    "return cash minused price when Buy and contract.status.notYet" in {
      user.calcAvailableCash( contract.copy( status = Contracts.Status.notYet ) ) must_== BigDecimal( 1000000 - ( 95d * 100 ) )
    }
    "return cash plused come back price when Buy and contract.status.impossible" in {
      user.calcAvailableCash( contract.copy( status = Contracts.Status.impossible ) ) must_== BigDecimal( 1000000 + ( 95d * 100 ) )
    }

    "return cash plused sold price when Sell and contract.status.done" in {
      user.calcAvailableCash( contract.copy( status = Contracts.Status.done, bos = transaction.BoS.sell ) ) must_== BigDecimal( 1000000 + ( 95d * 100 ) )
    }
    "return same cash when Sell and contract.status.notYet" in {
      user.calcAvailableCash( contract.copy( status = Contracts.Status.notYet, bos = transaction.BoS.sell ) ) must_== BigDecimal( 1000000 )
    }
    "return same cash when Sell and contract.status.impossible" in {
      user.calcAvailableCash( contract.copy( status = Contracts.Status.impossible, bos = transaction.BoS.sell ) ) must_== BigDecimal( 1000000 )
    }
    "return cash plused profit when Sell and contract.status.done" in {
      user.calcAvailableCash( contract.copy( status = Contracts.Status.done, bos = transaction.BoS.sell, account = transaction.Account.credit ) ) must_== BigDecimal( 1000000 + ( -5d * 100 ) )
    }
    "return cash plused arbitrage between sold price and buyed price when Sell and contract.status.done" in {
      user.calcAvailableCash( contract.copy(
        code = "1333", status = Contracts.Status.done, bos = transaction.BoS.sell, sol = transaction.SoL.short, account = transaction.Account.credit
      ) ) must_== BigDecimal( 1000000 + ( 200d - 95d ) * 100 )
    }
  }

  "Account.calcAvailableCredit" should {

    val contract = Contract (
      userId     = "111111",
      code       = "1332",
      account    = transaction.Account.credit,
      sol        = transaction.SoL.long,
      how        = transaction.How.limit,
      price      = 95d,
      volume     = 100,
      expiration = marketNow,
      bos        = transaction.BoS.buy,
      market     = "TYO"
    )

    val user = Account(
      userId     = "111111",
      userName        = "test",
      availableCash   = BigDecimal( 1000000 ),
      availableCredit = BigDecimal( 1000000 ),
      balance         = BigDecimal( 1000000 ),
      loan            = BigDecimal( 0 ),
      holdings        = List( holding1, holding2 ),
      contracted      = List.empty[Contract],
      notContracted   = List( contract.copy( status = Contracts.Status.notYet ) )
    )

    "return same credit when Buy and contract.status.done" in {
      user.calcAvailableCredit( contract.copy( status = Contracts.Status.done ) ) must_== BigDecimal( 1000000 )
    }
    "return credit minused price when Buy and contract.status.notYet" in {
      user.calcAvailableCredit( contract.copy( status = Contracts.Status.notYet ) ) must_== BigDecimal( 1000000 - ( 95d * 100 ) )
    }
    "return credit plused come back price when Buy and contract.status.impossible" in {
      user.calcAvailableCredit( contract.copy( status = Contracts.Status.impossible ) ) must_== BigDecimal( 1000000 + ( 95d * 100 ) )
    }

    "return credit plused sold price when Sell and contract.status.done" in {
      user.calcAvailableCredit( contract.copy( status = Contracts.Status.done, bos = transaction.BoS.sell ) ) must_== BigDecimal( 1000000 + ( 100d * 100 ) )
    }
    "return same credit when Sell and contract.status.notYet" in {
      user.calcAvailableCredit( contract.copy( status = Contracts.Status.notYet, bos = transaction.BoS.sell ) ) must_== BigDecimal( 1000000 )
    }
    "return same credit when Sell and contract.status.impossible" in {
      user.calcAvailableCredit( contract.copy( status = Contracts.Status.impossible, bos = transaction.BoS.sell ) ) must_== BigDecimal( 1000000 )
    }

    /** short */
    val contract2 = Contract (
      userId     = "111111",
      code       = "1332",
      account    = transaction.Account.credit,
      sol        = transaction.SoL.short,
      how        = transaction.How.limit,
      price      = 95d,
      volume     = 100,
      expiration = marketNow,
      bos        = transaction.BoS.buy,
      market     = "TYO"
    )

    "return same credit when Buy and contract.status.done" in {
      user.calcAvailableCredit( contract2.copy( status = Contracts.Status.done ) ) must_== BigDecimal( 1000000 )
    }
    "return credit minused price when Buy and contract.status.notYet" in {
      user.calcAvailableCredit( contract2.copy( status = Contracts.Status.notYet ) ) must_== BigDecimal( 1000000 - ( 95d * 100 ) )
    }
    "return credit plused come back price when Buy and contract.status.impossible" in {
      user.calcAvailableCredit( contract2.copy( status = Contracts.Status.impossible ) ) must_== BigDecimal( 1000000 + ( 95d * 100 ) )
    }

    "return credit plused arbitrage between sold price and buyed price when Sell and contract.status.done" in {
      user.calcAvailableCredit( contract2.copy( code = "1333", status = Contracts.Status.done, bos = transaction.BoS.sell ) ) must_== BigDecimal( 1000000 + 200d * 100 )
    }
    "return same credit when Sell and contract.status.notYet" in {
      user.calcAvailableCredit( contract2.copy( status = Contracts.Status.notYet, bos = transaction.BoS.sell ) ) must_== BigDecimal( 1000000 )
    }
    "return same credit when Sell and contract.status.impossible" in {
      user.calcAvailableCredit( contract2.copy( status = Contracts.Status.impossible, bos = transaction.BoS.sell ) ) must_== BigDecimal( 1000000 )
    }
  }

  "Account.calcHoldings" should {
    val contract = Contract (
      userId     = "111111",
      code       = "1332",
      account    = transaction.Account.cash,
      sol        = transaction.SoL.long,
      how        = transaction.How.limit,
      price      = 95d,
      volume     = 100,
      expiration = marketNow,
      bos        = transaction.BoS.buy,
      market     = "TYO",
      status     = Contracts.Status.done
    )
    val holding3 = Holding(
      userId = "111111",
      time   = marketNow,
      market = "TYO",
      code   = "1332",
      price  = BigDecimal(110),
      volume = 100,
      soL    = transaction.SoL.short
    )
    val user = Account(
      userId     = "111111",
      userName        = "test",
      availableCash   = BigDecimal( 1000000 ),
      availableCredit = BigDecimal( 1000000 ),
      balance         = BigDecimal( 1000000 ),
      loan            = BigDecimal( 0 ),
      holdings        = List( holding1, holding2, holding3 ),
      contracted      = List.empty[Contract],
      notContracted   = List( contract.copy( status = Contracts.Status.notYet ) )
    )
    "return sumed up holdings when adding the new stock." in {
      val stocks = user.calcHoldings( contract.copy( code = "1335" ) )
      val stock  = stocks.find( _.code == "1335" ).get

      stocks.size  must_== 4
      stock.volume must_== 100
      stock.price  must_== BigDecimal( 95d )
    }
    "return average price when adding the same stock by different price." in {
      val stocks = user.calcHoldings( contract )
      val stock  = stocks.find( s => s.code == contract.code && s.soL == contract.sol ).get

      stocks.size  must_== 3
      stock.volume must_== 300
      stock.price  must_== BigDecimal( ( 100d * 200 + 95 * 100 ) / 300 ).setScale( 3, BigDecimal.RoundingMode.HALF_UP )
    }
    "return redundunt volume when selling the half of the stock ." in {
      val stocks = user.calcHoldings( contract.copy( bos = transaction.BoS.sell ) )
      val stock  = stocks.find( s => s.code == contract.code && s.soL == contract.sol ).get

      stocks.size  must_== 3
      stock.volume must_== 100
      stock.price  must_== BigDecimal( 100d )
    }
    "return no stock when selling the full of the stock ." in {
      val stocks = user.calcHoldings( contract.copy( bos = transaction.BoS.sell, volume = 200 ) )
      val stock  = stocks.find( s => s.code == contract.code && s.soL == contract.sol )

      stocks.size  must_== 2
      stock must beNone
    }
    "return only the short stocks when selling the stocks of long." in {
      val stocks = user.calcHoldings( contract.copy( bos = transaction.BoS.sell, volume = 200 ) )
      val stock  = stocks.find( s => s.code == contract.code ).get

      stocks.size  must_== 2
      stock.soL must_== transaction.SoL.short
      stock.volume must_== 100
      stock.price  must_== BigDecimal( 110d )
    }
  }

  "Account.reBalance" should {
    val now = timestampFormat.parseDateTime( "2016-01-01 03:00:00" )

    Candles.candles1d += "1332" -> Vector(
      Candle(
        timestampFormat.parseDateTime( "2016-01-01 00:00:00" ).toDate,
        "TYO",
        "1332",
        BigDecimal( 500 ),
        BigDecimal( 500 ),
        BigDecimal( 500 ),
        BigDecimal( 500 ),
        123400L
      )
    )
    Candles.candles1d += "1333" -> Vector(
      Candle(
        timestampFormat.parseDateTime( "2016-01-01 00:00:00" ).toDate,
        "TYO",
        "1333",
        BigDecimal( 500 ),
        BigDecimal( 500 ),
        BigDecimal( 500 ),
        BigDecimal( 500 ),
        123400L
      )
    )

    val user = Account(
      userId     = "111111",
      userName        = "test",
      availableCash   = BigDecimal( 2980000 ),
      availableCredit = BigDecimal( 5960000 ),
      balance         = BigDecimal( 0 ),
      loan            = BigDecimal( 0 ),
      holdings        = List( holding1, holding2 ),
      contracted      = List.empty[Contract],
      notContracted   = List.empty[Contract]
    )
    "return 100,000 + 1000,000 when close of 1332 and 1333 are 500 yen." in {
      val profitLong  = ( 500 - 100 ) * 200
      val profitShort = ( 200 - 500 ) * 200
      val newUser = user.reBalance( now )
      newUser.balance must_== ( 200 * 200 ) + ( 100 * 200 ) + profitLong + profitShort + ( user.availableCash + user.availableCredit ) - 9000000
    }
  }

  "Account.toJson" should {
    val contract = Contract (
      userId     = "111111",
      code       = "1332",
      account    = transaction.Account.cash,
      sol        = transaction.SoL.long,
      how        = transaction.How.limit,
      price      = 95d,
      volume     = 100,
      expiration = marketNow,
      bos        = transaction.BoS.buy,
      market     = "TYO",
      status     = Contracts.Status.done
    )
    val user = Account(
      userId     = "111111",
      userName        = "test",
      availableCash   = BigDecimal( 1000000 ),
      availableCredit = BigDecimal( 1000000 ),
      balance         = BigDecimal( 1000000 ),
      loan            = BigDecimal( 0 ),
      holdings        = List( holding1, holding2 ),
      contracted      = List.empty[Contract],
      notContracted   = List( contract.copy( status = Contracts.Status.notYet ) )
    )
    "return customized formatted json string." in {
      val json = user.toJson
      println( json )
      json must not beEmpty
    }
  }
}
