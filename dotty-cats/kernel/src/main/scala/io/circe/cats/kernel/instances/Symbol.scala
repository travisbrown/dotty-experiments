package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Hash, LowerBounded, Order, PartialOrder}

private[kernel] object SymbolInstance extends Order[Symbol] with Hash[Symbol] with LowerBounded[Symbol] {
  override def minBound: Symbol = Symbol("")
  def hash(x: Symbol): Int = x.hashCode()

  override def eqv(x: Symbol, y: Symbol): Boolean =
    // Symbols are interned
    x eq y

  def compare(x: Symbol, y: Symbol): Int =
    if (x eq y) 0 else x.name.compareTo(y.name)

  def partialOrder: PartialOrder[Symbol] = this
}
