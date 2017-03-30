package com.ovoenergy.comms.templates.cache

import org.scalatest.{FlatSpec, Matchers}

class CachingStrategySpec extends FlatSpec with Matchers {

  val cachingStrategy = CachingStrategy.caffeine[String, Integer](maximumSize = 10)

  it should "memoize using a cache" in {
    var timesCalled = 0

    for (i <- 1 to 5) {
      val result = cachingStrategy.get("a key") {
        timesCalled += 1
        42 + i
      }
      result shouldBe 43
    }
    timesCalled shouldBe 1

    for (i <- 1 to 5) {
      val result = cachingStrategy.get("different key") {
        timesCalled += 1
        99 + i
      }
      result shouldBe 100
    }
    timesCalled shouldBe 2
  }

}
