package io.circe.cats.kernel

import io.circe.cats.kernel.instances.TupleEqInstances
import scala.collection.immutable.{Queue, SortedMap}
import scala.math.Equiv
import scala.specialized

/**
 * A type class used to determine equality between 2 instances of the same
 * type. Any 2 instances `x` and `y` are equal if `eqv(x, y)` is `true`.
 * Moreover, `eqv` should form an equivalence relation.
 */
trait Eq[@specialized A] extends Any with Serializable { self =>

  /**
   * Returns `true` if `x` and `y` are equivalent, `false` otherwise.
   */
  def eqv(x: A, y: A): Boolean

  /**
   * Returns `false` if `x` and `y` are equivalent, `true` otherwise.
   */
  def neqv(x: A, y: A): Boolean = !eqv(x, y)
}

private[kernel] abstract class EqFunctions[E[T] <: Eq[T]] {
  def eqv[@specialized A](x: A, y: A) given (A: E[A]): Boolean =
    A.eqv(x, y)

  def neqv[@specialized A](x: A, y: A) given (A: E[A]): Boolean =
    A.neqv(x, y)

}

object Eq extends EqFunctions[Eq] with TupleEqInstances {

  /**
   * Access a given `Eq[A]`.
   */
  final def apply[A] given (A: Eq[A]): Eq[A] = A

  /**
   * Convert a given `Eq[B]` to an `Eq[A]` using the given
   * function `f`.
   */
  def by[@specialized A, @specialized B](f: A => B) given (B: Eq[B]): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = B.eqv(f(x), f(y))
    }

  /**
   * Return an Eq that gives the result of the and of eq1 and eq2
   * note this is idempotent
   */
  def and[@specialized A](eq1: Eq[A], eq2: Eq[A]): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = eq1.eqv(x, y) && eq2.eqv(x, y)
    }

  /**
   * Return an Eq that gives the result of the or of this and that
   * Note this is idempotent
   */
  def or[@specialized A](eq1: Eq[A], eq2: Eq[A]): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = eq1.eqv(x, y) || eq2.eqv(x, y)
    }

  /**
   * Create an `Eq` instance from an `eqv` implementation.
   */
  def instance[A](f: (A, A) => Boolean): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = f(x, y)
    }

  /**
   * An `Eq[A]` that delegates to universal equality (`==`).
   *
   * This can be useful for case classes, which have reasonable `equals`
   * implementations
   */
  def fromUniversalEquals[A]: Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = x == y
    }

  /**
   * Everything is the same
   */
  def allEqual[A]: Eq[A] = new Eq[A] {
    def eqv(x: A, y: A) = true
  }

  /**
   * This is a monoid that creates an Eq that
   * checks that all equality checks pass
   */
  def allEqualBoundedSemilattice[A]: BoundedSemilattice[Eq[A]] = new BoundedSemilattice[Eq[A]] {
    def empty = allEqual[A]
    def combine(e1: Eq[A], e2: Eq[A]): Eq[A] = Eq.and(e1, e2)
    override def combineAllOption(es: TraversableOnce[Eq[A]]): Option[Eq[A]] =
      if (es.toIterator.isEmpty) None
      else {
        val materialized = es.toIterator.toVector
        Some(new Eq[A] {
          def eqv(x: A, y: A) = materialized.forall(_.eqv(x, y))
        })
      }
  }

  /**
   * This is a monoid that creates an Eq that
   * checks that at least one equality check passes
   */
  def anyEqualSemilattice[A]: Semilattice[Eq[A]] = new Semilattice[Eq[A]] {
    def combine(e1: Eq[A], e2: Eq[A]): Eq[A] = Eq.or(e1, e2)
    override def combineAllOption(es: TraversableOnce[Eq[A]]): Option[Eq[A]] =
      if (es.toIterator.isEmpty) None
      else {
        val materialized = es.toIterator.toVector
        Some(new Eq[A] {
          def eqv(x: A, y: A) = materialized.exists(_.eqv(x, y))
        })
      }
  }


  /**
   * Derive a `scala.math.Equiv[A]` from a `Eq[A]`
   * instance.
   */
  given [A] as Equiv[A] given (A: Eq[A]) {
    def equiv(a: A, b: A): Boolean = A.eqv(a, b)
  }

  given [A, B] as Eq[(A, B)] given Eq[A], Eq[B] {
    def eqv(xs: (A, B), ys: (A, B)): Boolean = {
      the[Eq[A]].eqv(xs._1, ys._1) && the[Eq[B]].eqv(xs._2, ys._2)
    }
  }

  given as Eq[String] {
    def eqv(x: String, y: String): Boolean = x == y
  }

  given as Eq[Boolean] {
    def eqv(x: Boolean, y: Boolean): Boolean = x == y
  }

  given [A] as Eq[A] given (A: Order[A]) = A
  given [A] as Eq[A] given (A: PartialOrder[A]) = A

  given [A] as Eq[Option[A]] given Eq[A] = io.circe.cats.kernel.instances.OptionEq[A]
  given [A] as Eq[List[A]] given Eq[A] = io.circe.cats.kernel.instances.ListEq[A]
  given [A] as Eq[Vector[A]] given Eq[A] = io.circe.cats.kernel.instances.VectorEq[A]
  given [A] as Eq[Stream[A]] given Eq[A] = io.circe.cats.kernel.instances.StreamEq[A]
  given [A] as Eq[Queue[A]] given Eq[A] = io.circe.cats.kernel.instances.QueueEq[A]
  given [K, V] as Eq[Map[K, V]] given Eq[V] = io.circe.cats.kernel.instances.MapEq[K, V]
  given [K, V] as Eq[SortedMap[K, V]] given Order[K], Eq[V] = io.circe.cats.kernel.instances.SortedMapEq[K, V]
  given [A, B] as Eq[Either[A, B]] given Eq[A], Eq[B] = io.circe.cats.kernel.instances.EitherEq[A, B]
}