package com.inkenkun.x1.virtual.market.redis

import com.redis._

trait Handler {

  import com.inkenkun.x1.virtual.market.config

  private val redisClient = new RedisClient( config.getString( "redis.host" ), config.getInt( "redis.port" ) )

  def setToRedis( key: String, value: String ): Boolean = redisClient.set( key, value )

  def getFromRedis( key: String ): Option[String] = redisClient.get( key )
}
