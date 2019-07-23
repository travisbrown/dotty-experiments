package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{BoundedSemilattice, CommutativeGroup, Hash, LowerBounded, Order, PartialOrder, UpperBounded}

private[kernel] object UnitInstance extends Order[Unit] with Hash[Unit] with LowerBounded[Unit] with UpperBounded[Unit] with CommutativeGroup[Unit] with BoundedSemilattice[Unit] {
  def compare(x: Unit, y: Unit): Int = 0
  def hash(x: Unit): Int = 0

  override def minBound: Unit = ()
  override def maxBound: Unit = ()


  override def eqv(x: Unit, y: Unit): Boolean = true
  override def neqv(x: Unit, y: Unit): Boolean = false
  override def gt(x: Unit, y: Unit): Boolean = false
  override def lt(x: Unit, y: Unit): Boolean = false
  override def gteqv(x: Unit, y: Unit): Boolean = true
  override def lteqv(x: Unit, y: Unit): Boolean = true

  override def min(x: Unit, y: Unit): Unit = ()
  override def max(x: Unit, y: Unit): Unit = ()

  override def partialOrder: PartialOrder[Unit] = this

  def empty: Unit = ()
  def combine(x: Unit, y: Unit): Unit = ()
  override def remove(x: Unit, y: Unit): Unit = ()
  def inverse(x: Unit): Unit = ()
  override protected[this] def repeatedCombineN(a: Unit, n: Int): Unit = ()
  override def combineAllOption(as: TraversableOnce[Unit]): Option[Unit] =
    if (as.toIterator.isEmpty) None else Some(())
}
