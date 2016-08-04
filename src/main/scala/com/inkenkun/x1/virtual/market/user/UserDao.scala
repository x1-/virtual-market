package com.inkenkun.x1.virtual.market.user

import scala.util.Try

import com.inkenkun.x1.virtual.market.mysql.{Handler => MySQLHandler}
import com.inkenkun.x1.virtual.market.redis.{Handler => RedisHandler}

object UserDao extends MySQLHandler with RedisHandler {
  
  import scalikejdbc._
  import com.inkenkun.x1.virtual.market.userId
  import com.inkenkun.x1.virtual.market.implicits._

  private val prefixWith: String => String = { s => "user_%s".format( s ) }

  private[user] def retrieve( userId: userId ): Account =
    getFromRedis( prefixWith( userId ) ).map( s => s.parseAs[Account] ).getOrElse( Account() )

  private[user] def update( account: Account ): Try[Boolean] =
    Try( setToRedis( prefixWith( account.userId ), account.toJson ) )

  private[user] def simpleFetchAll: List[Account] = DB readOnly { implicit session =>
    sql"""
      select * from
        accounts
    """.map( rs => Account( rs ) ).list.apply()
  }

  private[user] def fullFetchAll: List[Account] = DB readOnly { implicit session =>
    simpleFetchAll.map { ac =>
      val holdings = sql"""
        select * from
          account_holdings
        where
          user_id = ${ac.userId}
      """.map( rs => Holding( rs ) ).list.apply()
      val contracts = sql"""
        select * from
          account_contracts
        where
          user_id = ${ac.userId}
      """.map( rs => Contract( rs ) ).list().apply()

      ac.copy(
        holdings      = holdings,
        contracted    = contracts.filter( _.status != Contracts.Status.notYet ),
        notContracted = contracts.filter( _.status == Contracts.Status.notYet )
      )
    }
  }

  private[user] def expand( accounts: List[Account] ): Try[Unit] = { Try{
    accounts.foreach { acc =>
      setToRedis( prefixWith( acc.userId ), acc.toJson )
    }
  }}

  private[user] def add( account: Account ): Try[Long] = {
    setToRedis( prefixWith( account.userId ), account.toJson )

    DB localTx { implicit session =>
      Try( sql"""
        insert into accounts(
          user_id, user_name, available_cash, available_credit, balance
        ) values (
          ${account.userId}, ${account.userName}, ${account.availableCash}, ${account.availableCredit}, ${account.balance}
        );
      """.updateAndReturnGeneratedKey.apply() )
    }
  }
  
  private[user] def reset( userId: userId ): Try[Unit] = { Try{
    getFromRedis( userId ) match {
      case Some(a) =>
        val oldA = a.parseAs[Account]
        val newA = Account( userId = oldA.userId, userName = oldA.userName )
        setToRedis( userId, newA.toJson )
      case None => {}
    }
  }}

  private[user] def save: Unit = fullFetchAll.foreach { ac =>
    val account = retrieve( ac.userId )

    DB localTx { implicit session => Try {
      sql"""
         update accounts set
            available_cash   = ${account.availableCash}
           ,available_credit = ${account.availableCredit}
           ,balance          = ${account.balance}
           ,updated_at       = CURRENT_TIMESTAMP()
         where
           user_id = ${account.userId}
        ;
      """.update().apply()

      ac.holdings.foreach { holding =>
        sql"""
           delete from
             account_holdings
           where
             user_id = ${holding.userId}
           ;
        """.update().apply()

        sql"""
           insert into account_holdings (
              user_id
             ,time
             ,market
             ,code
             ,price
             ,volume
             ,short_or_long
           ) values (
              ${holding.userId}
             ,${holding.time}
             ,${holding.market}
             ,${holding.code}
             ,${holding.price}
             ,${holding.volume}
             ,${holding.soL.value}
           );
        """.update().apply()
      }

      ( ac.contracted ++ ac.notContracted ).foreach { contract => contract.id match {
        case Some( n ) =>
          sql"""
             update account_contracts set
                price  = ${contract.price}
               ,volume = ${contract.volume}
               ,status = ${contract.status.value}
             where
               id = ${n}
             ;
          """.update().apply()
        case None =>
          sql"""
             insert into account_contracts (
                user_id
               ,job_id
               ,market
               ,code
               ,price
               ,volume
               ,account
               ,short_or_long
               ,how
               ,buy_or_sell
               ,expiration
               ,status
             ) values (
                ${contract.userId}
               ,${contract.jobId}
               ,${contract.market}
               ,${contract.code}
               ,${contract.price}
               ,${contract.volume}
               ,${contract.account.value}
               ,${contract.sol.value}
               ,${contract.how.value}
               ,${contract.bos.value}
               ,${contract.expiration}
               ,${contract.status.value}
             );
          """.update().apply()
      }}
    }}
  }
}
