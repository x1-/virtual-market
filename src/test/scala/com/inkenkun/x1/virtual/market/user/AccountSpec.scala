package com.inkenkun.x1.virtual.market.user

import org.specs2.mutable.Specification

class AccountSpec extends Specification {
  
  import com.inkenkun.x1.virtual.market._
  import implicits._

  val holding1 = Holding(
    time   = marketNow,
    market = "TYO",
    code   = "1332",
    price  = BigDecimal(100),
    volume = 200,
    soL    = transaction.SoL.long
  )
  val holding2 = Holding(
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
      user.calcAvailableCredit( contract.copy( status = Contracts.Status.done, bos = transaction.BoS.sell ) ) must_== BigDecimal( 1000000 + ( 95d * 100 ) )
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
      user.calcAvailableCredit( contract2.copy( status = Contracts.Status.done, bos = transaction.BoS.sell ) ) must_== BigDecimal( 1000000 + ( 200d - 95d ) * 100 )
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
    val user = Account(
      userId     = "111111",
      userName        = "test",
      availableCash   = BigDecimal( 1000000 ),
      availableCredit = BigDecimal( 1000000 ),
      balance         = BigDecimal( 1000000 ),
      holdings        = List( holding1, holding2 ),
      contracted      = List.empty[Contract],
      notContracted   = List( contract.copy( status = Contracts.Status.notYet ) )
    )
    "return sumed up holdings when adding the new stock." in {
      val stocks = user.calcHoldings( contract.copy( code = "1335" ) )
      val stock  = stocks.find( _.code == "1335" ).get

      stocks.size  must_== 3
      stock.volume must_== 100
      stock.price  must_== BigDecimal( 95d )
    }
    "return average price when adding the same stock by different price." in {
      val stocks = user.calcHoldings( contract )
      val stock  = stocks.find( _.code == contract.code ).get

      stocks.size  must_== 2
      stock.volume must_== 300
      stock.price  must_== BigDecimal( ( 100d * 200 + 95 * 100 ) / 300 ).setScale( 3, BigDecimal.RoundingMode.HALF_UP )
    }
    "return redundunt volume when selling the half of the stock ." in {
      val stocks = user.calcHoldings( contract.copy( bos = transaction.BoS.sell ) )
      val stock  = stocks.find( _.code == contract.code ).get

      stocks.size  must_== 2
      stock.volume must_== 100
      stock.price  must_== BigDecimal( 100d )
    }
    "return no stock when selling the full of the stock ." in {
      val stocks = user.calcHoldings( contract.copy( bos = transaction.BoS.sell, volume = 200 ) )
      val stock  = stocks.find( _.code == contract.code )

      stocks.size  must_== 1
      stock must beNone
    }
  }
}
