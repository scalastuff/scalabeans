package org.scalastuff.util
import com.google.common.collect.MapMaker
import scala.collection.mutable.HashMap
import java.util.concurrent.ConcurrentMap

object WeakKeysMemo extends ConcurrentMapMemo {
  protected def createCache[K, V]: ConcurrentMap[K, V] = new MapMaker().weakKeys().makeMap[K, V]()
}

object WeakValuesMemo extends ConcurrentMapMemo {
  protected def createCache[K, V]: ConcurrentMap[K, V] = new MapMaker().weakValues().makeMap[K, V]()
}

object ConcurrentMapMemo extends ConcurrentMapMemo {
  protected def createCache[K, V]: ConcurrentMap[K, V] = new MapMaker().makeMap[K, V]()
}


object HashMapMemo extends Memo {
  def memo[K, V]: Memoizable[K, V] = new Memoizable[K, V] {
    val cache = new HashMap[K, V]
    def apply(arg: K)(computation: => V): V = {
      cache.getOrElseUpdate(arg, computation)
    }
  }
}

trait Memoizable[K, V] {
  def apply(arg: K)(computation: => V): V
}

trait Memo {
  def memo[K, V]: Memoizable[K, V]

  def memoize[K, V](pf: PartialFunction[K, V]): PartialFunction[K, V] = new PartialFunction[K, V] {
    val memo = Memo.this.memo[K, V]

    def isDefinedAt(x: K): Boolean = pf.isDefinedAt(x)
    def apply(arg: K): V = memo(arg)(pf(arg))
  }
}

trait ConcurrentMapMemo extends Memo {
  def memo[K, V]: Memoizable[K, V] = new Memoizable[K, V] {
    val cache = createCache[K, V]
    def apply(arg: K)(computed: => V): V = {
      val cached = cache.get(arg)
      if (cached == null) {
        // parameter 'computed' is actually a function!
        val computedVal = computed
        cache.putIfAbsent(arg, computedVal)
        computedVal
      } else {
        cached
      }
    }
  }

  protected def createCache[K, V]: ConcurrentMap[K, V]
}