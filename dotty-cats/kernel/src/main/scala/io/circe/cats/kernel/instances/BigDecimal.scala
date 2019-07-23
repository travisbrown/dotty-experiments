package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{CommutativeGroup, Hash, Order, PartialOrder}

private[kernel] object BigDecimalInstance extends Order[BigDecimal] with Hash[BigDecimal] with CommutativeGroup[BigDecimal] {
  val empty: BigDecimal = BigDecimal(0)
  def combine(x: BigDecimal, y: BigDecimal): BigDecimal = x + y
  def inverse(x: BigDecimal): BigDecimal = -x
  override def remove(x: BigDecimal, y: BigDecimal): BigDecimal = x - y

  def hash(x: BigDecimal): Int = x.hashCode()

  def compare(x: BigDecimal, y: BigDecimal): Int = x.compare(y)

  override def eqv(x: BigDecimal, y: BigDecimal): Boolean = x == y
  override def neqv(x: BigDecimal, y: BigDecimal): Boolean = x != y
  override def gt(x: BigDecimal, y: BigDecimal): Boolean = x > y
  override def gteqv(x: BigDecimal, y: BigDecimal): Boolean = x >= y
  override def lt(x: BigDecimal, y: BigDecimal): Boolean = x < y
  override def lteqv(x: BigDecimal, y: BigDecimal): Boolean = x <= y

  override def min(x: BigDecimal, y: BigDecimal): BigDecimal = x.min(y)
  override def max(x: BigDecimal, y: BigDecimal): BigDecimal = x.max(y)
}
