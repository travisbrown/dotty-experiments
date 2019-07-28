package io.circe.cats.kernel

/**
 * An commutative group (also known as an abelian group) is a group
 * whose combine operation is commutative.
 */
trait CommutativeGroup[@specialized(Int, Long, Float, Double) A] extends Any with Group[A] with CommutativeMonoid[A]

object CommutativeGroup extends GroupFunctions[CommutativeGroup] {

  /**
   * Access a given `CommutativeGroup[A]`.
   */
  def apply[A] given (A: CommutativeGroup[A]): CommutativeGroup[A] = A
}
