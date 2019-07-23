package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{CommutativeMonoid, CommutativeSemigroup, Eq, Hash, Monoid, Order, Semigroup}

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

private[kernel] class SortedMapEq[K, V] given (K: Order[K], V: Eq[V]) extends Eq[SortedMap[K, V]] {
  def eqv(x: SortedMap[K, V], y: SortedMap[K, V]): Boolean =
    if (x eq y) true
    else
      x.size == y.size && x.forall {
        case (k, v1) =>
          y.get(k) match {
            case Some(v2) => V.eqv(v1, v2)
            case None     => false
          }
      }
}

private[kernel] class SortedMapHash[K, V] given (K: Hash[K], O: Order[K], V: Hash[V])
    extends SortedMapEq[K, V]
    with Hash[SortedMap[K, V]] {
  // adapted from [[scala.util.hashing.MurmurHash3]],
  // but modified standard `Any#hashCode` to `ev.hash`.
  import scala.util.hashing.MurmurHash3._
  def hash(x: SortedMap[K, V]): Int = {
    var a, b, n = 0
    var c = 1
    x.foreach {
      case (k, v) =>
        val h = StaticMethods.product2HashWithPrefix(K.hash(k), V.hash(v), "Tuple2")
        a += h
        b ^= h
        c = StaticMethods.updateUnorderedHashC(c, h)
        n += 1
    }
    var h = mapSeed
    h = mix(h, a)
    h = mix(h, b)
    h = mixLast(h, c)
    finalizeHash(h, n)
  }
}

private[kernel] class SortedMapCommutativeMonoid[K, V] given Order[K], CommutativeSemigroup[V]
    extends SortedMapMonoid[K, V]
    with CommutativeMonoid[SortedMap[K, V]]

private[kernel] class SortedMapMonoid[K, V] given (K: Order[K], V: Semigroup[V]) extends Monoid[SortedMap[K, V]] {
  def empty: SortedMap[K, V] = SortedMap.empty(K.toOrdering)

  def combine(xs: SortedMap[K, V], ys: SortedMap[K, V]): SortedMap[K, V] =
    if (xs.size <= ys.size) {
      xs.foldLeft(ys) {
        case (my, (k, x)) =>
          my.updated(k, Semigroup.maybeCombine(x, my.get(k)))
      }
    } else {
      ys.foldLeft(xs) {
        case (mx, (k, y)) =>
          mx.updated(k, Semigroup.maybeCombine(mx.get(k), y))
      }
    }
}
