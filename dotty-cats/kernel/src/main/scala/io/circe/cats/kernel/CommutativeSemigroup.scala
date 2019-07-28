package io.circe.cats.kernel

/**
 * CommutativeSemigroup represents a commutative semigroup.
 *
 * A semigroup is commutative if for all x and y, x |+| y === y |+| x.
 */
trait CommutativeSemigroup[@specialized(Int, Long, Float, Double) A] extends Any with Semigroup[A]

object CommutativeSemigroup extends SemigroupFunctions[CommutativeSemigroup] {

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
}
