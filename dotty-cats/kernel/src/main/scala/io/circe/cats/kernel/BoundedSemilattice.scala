package io.circe.cats.kernel

import scala.collection.immutable.{BitSet, SortedSet}

trait BoundedSemilattice[@specialized(Int, Long, Float, Double) A] extends Any with Semilattice[A] with CommutativeMonoid[A]

object BoundedSemilattice extends SemilatticeFunctions[BoundedSemilattice] {

  /**
   * Access a given `BoundedSemilattice[A]`.
   */
  def apply[@specialized(Int, Long, Float, Double) A] given (A: BoundedSemilattice[A]): BoundedSemilattice[A] =
    A

  /**
   * Create a `BoundedSemilattice` instance from the given function and empty value.
   */
  def instance[A](emptyValue: A, cmb: (A, A) => A): BoundedSemilattice[A] = new BoundedSemilattice[A] {
    override val empty: A = emptyValue

    override def combine(x: A, y: A): A = cmb(x, y)
  }
}
