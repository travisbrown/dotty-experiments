package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{CommutativeGroup, Hash, LowerBounded, Order, PartialOrder, UpperBounded}

private[kernel] object ByteInstance extends Order[Byte] with Hash[Byte] with LowerBounded[Byte] with UpperBounded[Byte] with CommutativeGroup[Byte] {
  def combine(x: Byte, y: Byte): Byte = (x + y).toByte
  def empty: Byte = 0
  def inverse(x: Byte): Byte = (-x).toByte
  override def remove(x: Byte, y: Byte): Byte = (x - y).toByte

  override def minBound: Byte = Byte.MinValue
  override def maxBound: Byte = Byte.MaxValue

  def hash(x: Byte): Int = x.hashCode()
  def compare(x: Byte, y: Byte): Int =
    java.lang.Byte.compare(x, y)

  override def eqv(x: Byte, y: Byte): Boolean = x == y
  override def neqv(x: Byte, y: Byte): Boolean = x != y
  override def gt(x: Byte, y: Byte): Boolean = x > y
  override def gteqv(x: Byte, y: Byte): Boolean = x >= y
  override def lt(x: Byte, y: Byte): Boolean = x < y
  override def lteqv(x: Byte, y: Byte): Boolean = x <= y

  override def min(x: Byte, y: Byte): Byte =
    java.lang.Math.min(x.toInt, y.toInt).toByte
  override def max(x: Byte, y: Byte): Byte =
    java.lang.Math.max(x.toInt, y.toInt).toByte

  override def partialOrder: PartialOrder[Byte] = this
}
