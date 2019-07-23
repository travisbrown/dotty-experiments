package io.circe.cats.kernel

import io.circe.cats.kernel.instances.TupleCommutativeSemigroupInstances
import scala.specialized

/**
 * CommutativeSemigroup represents a commutative semigroup.
 *
 * A semigroup is commutative if for all x and y, x |+| y === y |+| x.
 */
trait CommutativeSemigroup[@specialized(Int, Long, Float, Double) A] extends Any with Semigroup[A]

object CommutativeSemigroup extends SemigroupFunctions[CommutativeSemigroup] with TupleCommutativeSemigroupInstances {

  /**
   * Access a given `CommutativeSemigroup[A]`.
   */
  def apply[A] given (A: CommutativeSemigroup[A]): CommutativeSemigroup[A] = A

  /**
   * Create a `CommutativeSemigroup` instance from the given function.
   */
  def instance[A](cmb: (A, A) => A): CommutativeSemigroup[A] = new CommutativeSemigroup[A] {
    override def combine(x: A, y: A): A = cmb(x, y)
  }

  given [A] as CommutativeSemigroup[A] given (A: CommutativeMonoid[A]) = A
  given [A] as CommutativeSemigroup[() => A] given CommutativeSemigroup[A] = io.circe.cats.kernel.instances.Function0CommutativeSemigroup[A]
}
