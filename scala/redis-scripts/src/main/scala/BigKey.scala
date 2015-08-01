package com.github.joprice

import akka.actor._
import akka.util.Timeout
import scala.concurrent.duration._
import com.redis.RedisClient
import scala.util.Random

object BigKey {

  def main(args: Array[String]) = {
    implicit val system = ActorSystem("redis-client")
    implicit val executionContext = system.dispatcher
    implicit val timeout = Timeout(5 seconds)
    val client = RedisClient("localhost", 6379)
    val hashKey = Random.alphanumeric.take(10).mkString

    val keys = 1000

    val kb = 1024
    val mb = kb * 1024
    val size = 1 * kb

    // ~5mb of data in single key
    val numKeys = 5000

    (0 until numKeys).map { _ =>
      val key = Random.alphanumeric.take(10).mkString
      val value = Random.alphanumeric.take(size).mkString
      client.hset(hashKey, key, value)
    }

    println(s"create big hash at key $hashKey")

    system.shutdown()
  }

  def data(size: Int): Array[Byte] = {
    val data = Array.fill[Byte](size)(0)
    Random.nextBytes(data)
    data
  }

}
