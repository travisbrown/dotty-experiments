package io.circe.cats.kernel

/**
 * CommutativeMonoid represents a commutative monoid.
 *
 * A monoid is commutative if for all x and y, x |+| y === y |+| x.
 */
trait CommutativeMonoid[@specialized(Int, Long, Float, Double) A] extends Any with Monoid[A] with CommutativeSemigroup[A]

object CommutativeMonoid extends MonoidFunctions[CommutativeMonoid] {

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
}
