package io.circe.cats.kernel

import io.circe.cats.kernel.instances.TupleBoundedSemilatticeInstances
import scala.collection.immutable.{BitSet, SortedSet}
import scala.specialized

trait BoundedSemilattice[@specialized(Int, Long, Float, Double) A] extends Any with Semilattice[A] with CommutativeMonoid[A]

object BoundedSemilattice extends SemilatticeFunctions[BoundedSemilattice] with TupleBoundedSemilatticeInstances {

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

  given as BoundedSemilattice[Unit] = io.circe.cats.kernel.instances.UnitInstance
  given as BoundedSemilattice[BitSet] = io.circe.cats.kernel.instances.BitSetInstance
  given [A] as BoundedSemilattice[SortedSet[A]] given Order[A] = io.circe.cats.kernel.instances.SortedSetBoundedSemilattice[A]

  given [A] as BoundedSemilattice[Set[A]] {
    def empty: Set[A] = Set.empty
    def combine(x: Set[A], y: Set[A]): Set[A] = x | y
  }
}
