package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{BoundedSemilattice, Hash, Order}
import scala.collection.immutable.SortedSet

private[kernel] class SortedSetOrder[A] given (A: Order[A]) extends Order[SortedSet[A]] {
  def compare(a1: SortedSet[A], a2: SortedSet[A]): Int = {
    a1.size.compare(a2.size) match {
      case 0 => Order.compare(a1.toStream, a2.toStream)
      case x => x
    }
  }

  override def eqv(s1: SortedSet[A], s2: SortedSet[A]): Boolean = {
    s1.toStream.corresponds(s2.toStream)(A.eqv)
  }
}

private[kernel] class SortedSetHash[A] given (A: Hash[A]) extends Hash[SortedSet[A]] {
  import scala.util.hashing.MurmurHash3._

  // adapted from [[scala.util.hashing.MurmurHash3]],
  // but modified standard `Any#hashCode` to `ev.hash`.
  def hash(xs: SortedSet[A]): Int = {
    var a, b, n = 0
    var c = 1
    xs.foreach { x =>
      val h = A.hash(x)
      a += h
      b ^= h
      c = StaticMethods.updateUnorderedHashC(c, h)
      n += 1
    }
    var h = setSeed
    h = mix(h, a)
    h = mix(h, b)
    h = mixLast(h, c)
    finalizeHash(h, n)
  }

  override def eqv(s1: SortedSet[A], s2: SortedSet[A]): Boolean = {
    s1.toStream.corresponds(s2.toStream)(A.eqv)
  }
}

private[kernel] class SortedSetBoundedSemilattice[A] given (A: Order[A]) extends BoundedSemilattice[SortedSet[A]] {
  def empty: SortedSet[A] = SortedSet.empty(A.toOrdering)
  def combine(x: SortedSet[A], y: SortedSet[A]): SortedSet[A] = x | y
}
