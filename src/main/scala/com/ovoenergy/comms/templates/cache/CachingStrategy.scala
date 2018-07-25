package com.ovoenergy.comms.templates.cache

import com.github.benmanes.caffeine.cache.Caffeine

trait CachingStrategy[K <: AnyRef, V <: AnyRef] {

  def get(cacheKey: K)(load: => V): V

  def remove(cacheKey: K): Unit

}

object CachingStrategy {

  def noCache[K <: AnyRef, V <: AnyRef] = new CachingStrategy[K, V] {
    override def get(cacheKey: K)(load: => V): V = load

    override def remove(cacheKey: K): Unit = ()
  }

  def caffeine[K <: AnyRef, V <: AnyRef](maximumSize: Int) = new CachingStrategy[K, V] {
    private val cache = Caffeine
      .newBuilder()
      .maximumSize(maximumSize)
      .build[K, V]()

    override def get(cacheKey: K)(load: => V) = {
      cache.get(cacheKey, new java.util.function.Function[K, V] { def apply(k: K): V = load })
    }

    override def remove(cacheKey: K) = {
      cache.invalidate(cacheKey)
    }
  }

}
