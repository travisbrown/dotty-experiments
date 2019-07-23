package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Hash, LowerBounded, Order, PartialOrder, UpperBounded}
import java.util.UUID

private[kernel] object UUIDInstance extends Order[UUID] with Hash[UUID] with LowerBounded[UUID] with UpperBounded[UUID] {
  def compare(x: UUID, y: UUID): Int = x.compareTo(y)
  def hash(x: UUID): Int = x.hashCode()
  def partialOrder: PartialOrder[UUID] = this

  override def minBound: UUID = new UUID(Long.MinValue, Long.MinValue)
  override def maxBound: UUID = new UUID(Long.MaxValue, Long.MaxValue)
}
