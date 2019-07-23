package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{CommutativeGroup, Hash, LowerBounded, Order, PartialOrder, UpperBounded}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, FiniteDuration}

private[kernel] object FiniteDurationInstance extends Order[FiniteDuration] with Hash[FiniteDuration] with LowerBounded[FiniteDuration] with UpperBounded[FiniteDuration] with CommutativeGroup[FiniteDuration] {
  override def minBound: FiniteDuration = FiniteDuration(-Long.MaxValue, TimeUnit.NANOSECONDS)
  override def maxBound: FiniteDuration = FiniteDuration(Long.MaxValue, TimeUnit.NANOSECONDS)

  def hash(x: FiniteDuration): Int = x.hashCode()
  def compare(x: FiniteDuration, y: FiniteDuration): Int = x.compare(y)

  override def eqv(x: FiniteDuration, y: FiniteDuration): Boolean = x == y
  override def neqv(x: FiniteDuration, y: FiniteDuration): Boolean = x != y
  override def gt(x: FiniteDuration, y: FiniteDuration): Boolean = x > y
  override def gteqv(x: FiniteDuration, y: FiniteDuration): Boolean = x >= y
  override def lt(x: FiniteDuration, y: FiniteDuration): Boolean = x < y
  override def lteqv(x: FiniteDuration, y: FiniteDuration): Boolean = x <= y

  override def min(x: FiniteDuration, y: FiniteDuration): FiniteDuration = x.min(y)
  override def max(x: FiniteDuration, y: FiniteDuration): FiniteDuration = x.max(y)

  override def partialOrder: PartialOrder[FiniteDuration] = this

  def empty: FiniteDuration = Duration.Zero
  def inverse(x: FiniteDuration): FiniteDuration = -x
  def combine(x: FiniteDuration, y: FiniteDuration): FiniteDuration = x + y
  override def remove(x: FiniteDuration, y: FiniteDuration): FiniteDuration = x - y
}
