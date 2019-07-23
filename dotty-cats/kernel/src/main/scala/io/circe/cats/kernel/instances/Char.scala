package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Hash, LowerBounded, Order, PartialOrder, UpperBounded}

private[kernel] object CharInstance extends Order[Char] with Hash[Char] with LowerBounded[Char] with UpperBounded[Char] {
  override def minBound: Char = Char.MinValue
  override def maxBound: Char = Char.MaxValue

  def hash(x: Char): Int = x.hashCode()
  def compare(x: Char, y: Char): Int =
    java.lang.Character.compare(x, y)

  override def eqv(x: Char, y: Char): Boolean = x == y
  override def neqv(x: Char, y: Char): Boolean = x != y
  override def gt(x: Char, y: Char): Boolean = x > y
  override def gteqv(x: Char, y: Char): Boolean = x >= y
  override def lt(x: Char, y: Char): Boolean = x < y
  override def lteqv(x: Char, y: Char): Boolean = x <= y

  override def partialOrder: PartialOrder[Char] = this
}
