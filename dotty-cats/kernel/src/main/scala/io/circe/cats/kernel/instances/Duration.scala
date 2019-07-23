package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{CommutativeGroup, Hash, LowerBounded, Order, PartialOrder, UpperBounded}
import scala.concurrent.duration.Duration

private[kernel] object DurationInstance extends Order[Duration] with Hash[Duration] with LowerBounded[Duration] with UpperBounded[Duration] with CommutativeGroup[Duration] {
  override def minBound: Duration = Duration.MinusInf
  override def maxBound: Duration = Duration.Inf

  def hash(x: Duration): Int = x.hashCode()

  /**
   * This ordering is valid for all defined durations.
   *
   * The value Duration.Undefined breaks our laws, because undefined
   * values are not equal to themselves.
   */
  def compare(x: Duration, y: Duration): Int = x.compare(y)

  override def eqv(x: Duration, y: Duration): Boolean = x == y
  override def neqv(x: Duration, y: Duration): Boolean = x != y
  override def gt(x: Duration, y: Duration): Boolean = x > y
  override def gteqv(x: Duration, y: Duration): Boolean = x >= y
  override def lt(x: Duration, y: Duration): Boolean = x < y
  override def lteqv(x: Duration, y: Duration): Boolean = x <= y

  override def min(x: Duration, y: Duration): Duration = x.min(y)
  override def max(x: Duration, y: Duration): Duration = x.max(y)

  override def partialOrder: PartialOrder[Duration] = this

  /**
   * This group models addition, but has a few problematic edge cases.
   *
   *   1. finite values can overflow, throwing an exception
   *   2. inf + (-inf) = undefined, not zero
   *   3. undefined + zero = undefined
   */
  def empty: Duration = Duration.Zero
  def inverse(x: Duration): Duration = -x
  def combine(x: Duration, y: Duration): Duration = x + y
  override def remove(x: Duration, y: Duration): Duration = x - y
}
