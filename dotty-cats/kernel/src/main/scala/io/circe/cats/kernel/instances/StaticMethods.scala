package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Eq, Hash, Order, PartialOrder}
import scala.collection.mutable

private[cats] object StaticMethods {
  private[this] class WrappedMutableMap[K, V](m: mutable.Map[K, V]) extends Map[K, V] with Serializable {
    override def size: Int = m.size
    def get(k: K): Option[V] = m.get(k)
    def iterator: Iterator[(K, V)] = m.iterator
    def +[V2 >: V](kv: (K, V2)): Map[K, V2] = m.toMap + kv
    def -(key: K): Map[K, V] = m.toMap - key
    override def updated[V1 >: V](key: K, value: V1): Map[K, V1] = m.updated(key, value).toMap
  }

  def wrapMutableMap[K, V](m: mutable.Map[K, V]): Map[K, V] =
    new WrappedMutableMap(m)

  // adapted from [[scala.util.hashing.MurmurHash3]],
  // but modified standard `Any#hashCode` to `ev.hash`.
  def listHash[A](x: List[A]) given (A: Hash[A]): Int = {
    import scala.util.hashing.MurmurHash3._
    var n = 0
    var h = seqSeed
    var elems = x
    while (!elems.isEmpty) {
      val head = elems.head
      val tail = elems.tail
      h = mix(h, A.hash(head))
      n += 1
      elems = tail
    }
    finalizeHash(h, n)
  }

  // scalastyle:off return
  def iteratorCompare[A](xs: Iterator[A], ys: Iterator[A]) given (A: Order[A]): Int = {
    while (true) {
      if (xs.hasNext) {
        if (ys.hasNext) {
          val x = xs.next
          val y = ys.next
          val cmp = A.compare(x, y)
          if (cmp != 0) return cmp
        } else {
          return 1
        }
      } else {
        return if (ys.hasNext) -1 else 0
      }
    }
    0
  }

  def iteratorPartialCompare[A](xs: Iterator[A], ys: Iterator[A]) given (A: PartialOrder[A]): Double = {
    while (true) {
      if (xs.hasNext) {
        if (ys.hasNext) {
          val x = xs.next
          val y = ys.next
          val cmp = A.partialCompare(x, y)
          if (cmp != 0.0) return cmp
        } else {
          return 1.0
        }
      } else {
        return if (ys.hasNext) -1.0 else 0.0
      }
    }
    0.0
  }

  def iteratorEq[A](xs: Iterator[A], ys: Iterator[A]) given (A: Eq[A]): Boolean = {
    while (true) {
      if (xs.hasNext) {
        if (ys.hasNext) {
          if (A.neqv(xs.next, ys.next)) return false
        } else {
          return false
        }
      } else {
        return !ys.hasNext
      }
    }
    true
  }
  // scalastyle:on return

  def combineNIterable[A, R](b: mutable.Builder[A, R], x: Iterable[A], n: Int): R = {
    var i = n
    while (i > 0) { b ++= x; i -= 1 }
    b.result
  }

  def combineAllIterable[A, R](b: mutable.Builder[A, R], xs: TraversableOnce[Iterable[A]]): R = {
    xs.toIterator.foreach(b ++= _)
    b.result
  }

  // Adapted from scala.util.hashing.MurmurHash#productHash.
  def product1Hash(_1Hash: Int): Int = {
    import scala.util.hashing.MurmurHash3._
    var h = productSeed
    h = mix(h, _1Hash)
    finalizeHash(h, 1)
  }

  // Adapted from scala.util.hashing.MurmurHash#productHash.
  def product2Hash(_1Hash: Int, _2Hash: Int): Int = {
    import scala.util.hashing.MurmurHash3._
    var h = productSeed
    h = mix(h, _1Hash)
    h = mix(h, _2Hash)
    finalizeHash(h, 2)
  }

  private[kernel] def product1HashWithPrefix(_1Hash: Int, prefix: String): Int = {
    import scala.util.hashing.MurmurHash3._
    var h = productSeed
    h = mix(h, _1Hash)
    finalizeHash(h, 1)
  }

  // Adapted from scala.util.hashing.MurmurHash#productHash.
  private[cats] def product2HashWithPrefix(_1Hash: Int, _2Hash: Int, prefix: String): Int = {
    import scala.util.hashing.MurmurHash3._
    var h = productSeed
    h = mix(h, _1Hash)
    h = mix(h, _2Hash)
    finalizeHash(h, 2)
  }

  private[cats] def updateUnorderedHashC(c: Int, h: Int): Int = if (h != 0) c * h else c

    // adapted from scala.util.hashing.MurmurHash3
  def orderedHash[A](xs: TraversableOnce[A]) given (A: Hash[A]): Int = {
    import scala.util.hashing.MurmurHash3._
    var n = 0
    var h = seqSeed
    xs.foreach { x =>
      h = mix(h, A.hash(x))
      n += 1
    }
    finalizeHash(h, n)
  }
}
