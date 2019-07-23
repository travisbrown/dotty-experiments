package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{BoundedSemilattice, Hash, PartialOrder}
import scala.collection.immutable.BitSet

private[kernel] object BitSetInstance extends PartialOrder[BitSet] with Hash[BitSet] with BoundedSemilattice[BitSet] {
  def hash(x: BitSet): Int = x.hashCode()

  def partialCompare(x: BitSet, y: BitSet): Double =
    if (x eq y) 0.0
    else if (x.size < y.size) if (x.subsetOf(y)) -1.0 else Double.NaN
    else if (y.size < x.size) if (y.subsetOf(x)) 1.0 else Double.NaN
    else if (x == y) 0.0
    else Double.NaN

  override def eqv(x: BitSet, y: BitSet): Boolean =
    x == y
  
  def empty: BitSet = BitSet.empty
  def combine(x: BitSet, y: BitSet): BitSet = x | y
}
