package io.circe.cats.kernel

import io.circe.cats.kernel.instances.TupleCommutativeMonoidInstances
import scala.collection.immutable.SortedMap
import scala.specialized

/**
 * CommutativeMonoid represents a commutative monoid.
 *
 * A monoid is commutative if for all x and y, x |+| y === y |+| x.
 */
trait CommutativeMonoid[@specialized(Int, Long, Float, Double) A] extends Any with Monoid[A] with CommutativeSemigroup[A]

object CommutativeMonoid extends MonoidFunctions[CommutativeMonoid] with TupleCommutativeMonoidInstances with LowPriorityCommutativeMonoidInstances {

  /**
   * Access a given `CommutativeMonoid[A]`.
   */
  def apply[A] given (A: CommutativeMonoid[A]): CommutativeMonoid[A] = A

  /**
   * Create a `CommutativeMonoid` instance from the given function and empty value.
   */
  def instance[A](emptyValue: A, cmb: (A, A) => A): CommutativeMonoid[A] = new CommutativeMonoid[A] {
    override val empty: A = emptyValue
    override def combine(x: A, y: A): A = cmb(x, y)
  }

  given [A] as CommutativeMonoid[A] given (A: BoundedSemilattice[A]) = A

  given [K, V] as CommutativeMonoid[Map[K, V]] given Semigroup[V] = io.circe.cats.kernel.instances.MapCommutativeMonoid[K, V]
  given [K, V] as CommutativeMonoid[SortedMap[K, V]] given Order[K], CommutativeSemigroup[V] = io.circe.cats.kernel.instances.SortedMapCommutativeMonoid[K, V]
}

private trait LowPriorityCommutativeMonoidInstances {
  given [A] as CommutativeMonoid[A] given (A: CommutativeGroup[A]) = A  
}