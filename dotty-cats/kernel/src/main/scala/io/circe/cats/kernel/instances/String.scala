package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Hash, LowerBounded, Monoid, Order, PartialOrder}

private[kernel] object StringInstance extends Order[String] with Hash[String] with LowerBounded[String] with Monoid[String] {
  override def minBound: String = ""
  def hash(x: String): Int = x.hashCode()

  override def eqv(x: String, y: String): Boolean =
    x == y
  def compare(x: String, y: String): Int =
    if (x eq y) 0 else x.compareTo(y)

  override def partialOrder: PartialOrder[String] = this
  def empty: String = ""
  def combine(x: String, y: String): String = x + y

  override def combineAll(xs: TraversableOnce[String]): String = {
    val sb = new StringBuilder
    xs.toIterator.foreach(sb.append)
    sb.toString
  }
}
