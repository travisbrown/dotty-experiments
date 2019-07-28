package io.circe.cats.kernel

/**
 * Semilattices are commutative semigroups whose operation
 * (i.e. combine) is also idempotent.
 */
trait Semilattice[@specialized(Int, Long, Float, Double) A] extends Any with Band[A] with CommutativeSemigroup[A] { self =>

  /**
   * Given Eq[A], return a PartialOrder[A] using the `combine`
   * operator to determine the partial ordering. This method assumes
   * `combine` functions as `meet` (that is, as a lower bound).
   *
   * This method returns:
   *
   *    0.0 if x = y
   *   -1.0 if x = combine(x, y)
   *    1.0 if y = combine(x, y)
   *    NaN otherwise
   */
  def asMeetPartialOrder given (A: Eq[A]): PartialOrder[A] =
    new PartialOrder[A] {
      def partialCompare(x: A, y: A): Double =
        if (A.eqv(x, y)) 0.0
        else {
          val z = self.combine(x, y)
          if (A.eqv(x, z)) -1.0 else if (A.eqv(y, z)) 1.0 else Double.NaN
        }
    }

  /**
   * Given Eq[A], return a PartialOrder[A] using the `combine`
   * operator to determine the partial ordering. This method assumes
   * `combine` functions as `join` (that is, as an upper bound).
   *
   * This method returns:
   *
   *    0.0 if x = y
   *   -1.0 if y = combine(x, y)
   *    1.0 if x = combine(x, y)
   *    NaN otherwise
   */
  def asJoinPartialOrder given (A: Eq[A]): PartialOrder[A] =
    new PartialOrder[A] {
      def partialCompare(x: A, y: A): Double =
        if (A.eqv(x, y)) 0.0
        else {
          val z = self.combine(x, y)
          if (A.eqv(y, z)) -1.0 else if (A.eqv(x, z)) 1.0 else Double.NaN
        }
    }
}

private[kernel] abstract class SemilatticeFunctions[S[T] <: Semilattice[T]] extends SemigroupFunctions[S] {
  def asMeetPartialOrder[A] given S[A], Eq[A]: PartialOrder[A] =
    the[S[A]].asMeetPartialOrder
  def asJoinPartialOrder[A] given S[A], Eq[A]: PartialOrder[A] =
    the[S[A]].asJoinPartialOrder
}

object Semilattice extends SemilatticeFunctions[Semilattice] {

  /**
   * Access a given `Semilattice[A]`.
   */
  def apply[@specialized(Int, Long, Float, Double) A] given (A: Semilattice[A]): Semilattice[A] = A

  /**
   * Create a `Semilattice` instance from the given function.
   */
  def instance[A](cmb: (A, A) => A): Semilattice[A] = new Semilattice[A] {
    override def combine(x: A, y: A): A = cmb(x, y)
  }
}
