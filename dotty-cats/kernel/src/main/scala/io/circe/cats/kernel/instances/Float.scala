package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{CommutativeGroup, Hash, Order}

private[kernel] object FloatInstance extends Order[Float] with Hash[Float] with CommutativeGroup[Float] {
  def combine(x: Float, y: Float): Float = x + y
  def empty: Float = 0f
  def inverse(x: Float): Float = -x
  override def remove(x: Float, y: Float): Float = x - y

  def hash(x: Float): Int = x.hashCode()
  def compare(x: Float, y: Float): Int =
    java.lang.Float.compare(x, y)

  override def eqv(x: Float, y: Float): Boolean = x == y
  override def neqv(x: Float, y: Float): Boolean = x != y
  override def gt(x: Float, y: Float): Boolean = x > y
  override def gteqv(x: Float, y: Float): Boolean = x >= y
  override def lt(x: Float, y: Float): Boolean = x < y
  override def lteqv(x: Float, y: Float): Boolean = x <= y

  override def min(x: Float, y: Float): Float =
    Math.min(x, y)
  override def max(x: Float, y: Float): Float =
    Math.max(x, y)
}
