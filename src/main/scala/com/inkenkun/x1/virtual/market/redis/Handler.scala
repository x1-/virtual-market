package com.inkenkun.x1.virtual.market.redis

import com.redis._

object Handler {

  import com.inkenkun.x1.virtual.market.config

  val client = new RedisClient( config.getString( "redis.host" ), config.getInt( "redis.port" ) )

  def set( key: String, value: String ): Boolean = client.set( key, value )

  def get( key: String ): Option[String] = client.get( key )
}
