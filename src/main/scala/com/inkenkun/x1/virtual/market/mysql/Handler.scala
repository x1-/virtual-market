package com.inkenkun.x1.virtual.market.mysql

import scalikejdbc._
import com.inkenkun.x1.virtual.market._

trait Handler {

  Class.forName( "com.mysql.jdbc.Driver" )
  ConnectionPool.singleton( config.getString( "mysql.jdbc" ), config.getString( "mysql.user" ), config.getString( "mysql.passwd" ) )

  implicit val session = AutoSession
}
