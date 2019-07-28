package io.circe.cats.kernel

import java.lang.Double.isNaN

/**
 * The `PartialOrder` type class is used to define a partial ordering on some type `A`.
 *
 * A partial order is defined by a relation <=, which obeys the following laws:
 *
 * - x <= x (reflexivity)
 * - if x <= y and y <= x, then x = y (anti-symmetry)
 * - if x <= y and y <= z, then x <= z (transitivity)
 *
 * To compute both <= and >= at the same time, we use a Double number
 * to encode the result of the comparisons x <= y and x >= y.
 * The truth table is defined as follows:
 *
 * x <= y    x >= y      Double
 * true      true        = 0.0     (corresponds to x = y)
 * false     false       = NaN     (x and y cannot be compared)
 * true      false       = -1.0    (corresponds to x < y)
 * false     true        = 1.0     (corresponds to x > y)
 */
trait PartialOrder[@specialized A] extends Any with Eq[A] { self =>

  /**
   * Result of comparing `x` with `y`. Returns NaN if operands are not
   * comparable. If operands are comparable, returns a Double whose
   * sign is:
   * - negative iff `x < y`
   * - zero     iff `x = y`
   * - positive iff `x > y`
   */
  def partialCompare(x: A, y: A): Double

  /**
   * Like `partialCompare`, but returns a [[cats.kernel.Comparison]] instead of an Double.
   * Has the benefit of being able to pattern match on, but not as performant.
   */
  def partialComparison(x: A, y: A): Option[Comparison] =
    Comparison.fromDouble(partialCompare(x, y))

  /**
   * Result of comparing `x` with `y`. Returns None if operands are
   * not comparable. If operands are comparable, returns Some[Int]
   * where the Int sign is:
   * - negative iff `x < y`
   * - zero     iff `x = y`
   * - positive iff `x > y`
   */
  def tryCompare(x: A, y: A): Option[Int] = {
    val c = math.signum(partialCompare(x, y))
    if (isNaN(c)) None else Some(c.toInt)
  }

  /**
   * Returns Some(x) if x <= y, Some(y) if x > y, otherwise None.
   */
  def pmin(x: A, y: A): Option[A] = {
    val c = partialCompare(x, y)
    if (c <= 0) Some(x)
    else if (c > 0) Some(y)
    else None
  }

  /**
   * Returns Some(x) if x >= y, Some(y) if x < y, otherwise None.
   */
  def pmax(x: A, y: A): Option[A] = {
    val c = partialCompare(x, y)
    if (c >= 0) Some(x)
    else if (c < 0) Some(y)
    else None
  }

  // The following may be overridden for performance:

  /**
   * Returns true if `x` = `y`, false otherwise.
   */
  def eqv(x: A, y: A): Boolean = partialCompare(x, y) == 0

  /**
   * Returns true if `x` <= `y`, false otherwise.
   */
  def lteqv(x: A, y: A): Boolean = partialCompare(x, y) <= 0

  /**
   * Returns true if `x` < `y`, false otherwise.
   */
  def lt(x: A, y: A): Boolean = partialCompare(x, y) < 0

  /**
   * Returns true if `x` >= `y`, false otherwise.
   */
  def gteqv(x: A, y: A): Boolean = partialCompare(x, y) >= 0

  /**
   * Returns true if `x` > `y`, false otherwise.
   */
  def gt(x: A, y: A): Boolean = partialCompare(x, y) > 0
}

object PartialOrder {

  /**
   * Access a given `PartialOrder[A]`.
   */
  def apply[A] given (A: PartialOrder[A]): PartialOrder[A] = A

  /**
   * Convert a given `PartialOrder[B]` to an `PartialOrder[A]` using the given
   * function `f`.
   */
  def by[@specialized A, @specialized B](f: A => B) given (B: PartialOrder[B]): PartialOrder[A] =
    new PartialOrder[A] {
      def partialCompare(x: A, y: A): Double = B.partialCompare(f(x), f(y))
    }

  /**
   * Defines a partial order on `A` from p where all arrows switch direction.
   */
  def reverse[@specialized A](p: PartialOrder[A]): PartialOrder[A] =
    new PartialOrder[A] {
      def partialCompare(x: A, y: A): Double = p.partialCompare(y, x)
    }

  /**
   * Define a `PartialOrder[A]` using the given function `f`.
   */
  def from[@specialized A](f: (A, A) => Double): PartialOrder[A] =
    new PartialOrder[A] {
      def partialCompare(x: A, y: A) = f(x, y)
    }

  def fromPartialOrdering[A] given (A: PartialOrdering[A]): PartialOrder[A] = new PartialOrder[A] {
    def partialCompare(x: A, y: A): Double =
      A.tryCompare(x, y).fold(Double.NaN)(_.toDouble)
  }

  def partialCompare[@specialized A](x: A, y: A) given (A: PartialOrder[A]): Double =
    A.partialCompare(x, y)
  def tryCompare[@specialized A](x: A, y: A) given (A: PartialOrder[A]): Option[Int] =
    A.tryCompare(x, y)

  def pmin[@specialized A](x: A, y: A) given (A: PartialOrder[A]): Option[A] =
    A.pmin(x, y)
  def pmax[@specialized A](x: A, y: A) given (A: PartialOrder[A]): Option[A] =
    A.pmax(x, y)

  def lteqv[@specialized A](x: A, y: A) given (A: PartialOrder[A]): Boolean =
    A.lteqv(x, y)
  def lt[@specialized A](x: A, y: A) given (A: PartialOrder[A]): Boolean =
    A.lt(x, y)
  def gteqv[@specialized A](x: A, y: A) given (A: PartialOrder[A]): Boolean =
    A.gteqv(x, y)
  def gt[@specialized A](x: A, y: A) given (A: PartialOrder[A]): Boolean =
    A.gt(x, y)

  given [A] as PartialOrdering[A] given (A: PartialOrder[A]) {
    def tryCompare(x: A, y: A): Option[Int] = A.tryCompare(x, y)
    def lteq(x: A, y: A): Boolean = A.lteqv(x, y)
  }
}
