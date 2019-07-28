package io.circe.cats.kernel

import io.circe.cats.kernel.instances.TupleOrderInstances
import java.util.UUID
import scala.collection.immutable.{BitSet, Queue, SortedMap, SortedSet}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.math.Equiv
import scala.specialized

/**
 * A type class used to determine equality between 2 instances of the same
 * type. Any two instances `x` and `y` are equal if `eqv(x, y)` is `true`.
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

private[kernel] trait EqFunctions[E[T] <: Eq[T]] {
  def eqv[@specialized A](x: A, y: A) given (A: E[A]): Boolean =
    A.eqv(x, y)

  def neqv[@specialized A](x: A, y: A) given (A: E[A]): Boolean =
    A.neqv(x, y)

}

object Eq extends EqFunctions[Eq] with TupleOrderInstances with PartialOrderInstances {

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

  // `Set` and `BitSet` are special cases.
  given as (PartialOrder[BitSet] & Hash[BitSet]) = io.circe.cats.kernel.instances.BitSetInstance
  given [A] as (PartialOrder[Set[A]] & Hash[Set[A]]) = io.circe.cats.kernel.instances.SetInstance[A]

  given as (Order[Unit] & Hash[Unit]) = io.circe.cats.kernel.instances.UnitInstance
  given as (Order[Boolean] & Hash[Boolean]) = io.circe.cats.kernel.instances.BooleanInstance
  given as (Order[Byte] & Hash[Byte]) = io.circe.cats.kernel.instances.ByteInstance
  given as (Order[Short] & Hash[Short]) = io.circe.cats.kernel.instances.ShortInstance
  given as (Order[Int] & Hash[Int]) = io.circe.cats.kernel.instances.IntInstance
  given as (Order[Long] & Hash[Long]) = io.circe.cats.kernel.instances.LongInstance
  given as (Order[BigInt] & Hash[BigInt]) = io.circe.cats.kernel.instances.BigIntInstance
  given as (Order[BigDecimal] & Hash[BigDecimal]) = io.circe.cats.kernel.instances.BigDecimalInstance
  given as (Order[Duration] & Hash[Duration]) = io.circe.cats.kernel.instances.DurationInstance
  given as (Order[FiniteDuration] & Hash[FiniteDuration]) = io.circe.cats.kernel.instances.FiniteDurationInstance
  given as (Order[Char] & Hash[Char]) = io.circe.cats.kernel.instances.CharInstance
  given as (Order[Symbol] & Hash[Symbol]) = io.circe.cats.kernel.instances.SymbolInstance
  given as (Order[String] & Hash[String]) = io.circe.cats.kernel.instances.StringInstance
  given as (Order[UUID] & Hash[UUID]) = io.circe.cats.kernel.instances.UUIDInstance
  given as (Order[Double] & Hash[Double]) = io.circe.cats.kernel.instances.DoubleInstance
  given as (Order[Float] & Hash[Float]) = io.circe.cats.kernel.instances.FloatInstance

  given [A] as Order[Option[A]] given Order[A] = io.circe.cats.kernel.instances.OptionOrder[A]
  given [A] as Order[List[A]] given Order[A] = io.circe.cats.kernel.instances.ListOrder[A]
  given [A] as Order[Vector[A]] given Order[A] = io.circe.cats.kernel.instances.VectorOrder[A]
  given [A] as Order[Stream[A]] given Order[A] = io.circe.cats.kernel.instances.StreamOrder[A]
  given [A] as Order[Queue[A]] given Order[A] = io.circe.cats.kernel.instances.QueueOrder[A]
  given [A] as Order[SortedSet[A]] given Order[A] = io.circe.cats.kernel.instances.SortedSetOrder[A]
}

private trait PartialOrderInstances extends HashInstances {
  given [A] as PartialOrder[Option[A]] given PartialOrder[A] = io.circe.cats.kernel.instances.OptionPartialOrder[A]
  given [A] as PartialOrder[List[A]] given PartialOrder[A] = io.circe.cats.kernel.instances.ListPartialOrder[A]
  given [A] as PartialOrder[Vector[A]] given PartialOrder[A] = io.circe.cats.kernel.instances.VectorPartialOrder[A]
  given [A] as PartialOrder[Stream[A]] given PartialOrder[A] = io.circe.cats.kernel.instances.StreamPartialOrder[A]
  given [A] as PartialOrder[Queue[A]] given PartialOrder[A] = io.circe.cats.kernel.instances.QueuePartialOrder[A]
  given [A] as PartialOrder[() => A] given PartialOrder[A] = io.circe.cats.kernel.instances.Function0PartialOrder[A]
}

private trait HashInstances extends EqInstances {
  given [A] as Hash[Option[A]] given Hash[A] = io.circe.cats.kernel.instances.OptionHash[A]
  given [A] as Hash[List[A]] given Hash[A] = io.circe.cats.kernel.instances.ListHash[A]
  given [A] as Hash[Vector[A]] given Hash[A] = io.circe.cats.kernel.instances.VectorHash[A]
  given [A] as Hash[Stream[A]] given Hash[A] = io.circe.cats.kernel.instances.StreamHash[A]
  given [A] as Hash[Queue[A]] given Hash[A] = io.circe.cats.kernel.instances.QueueHash[A]
  given [K, V] as Hash[Map[K, V]] given Hash[V] = io.circe.cats.kernel.instances.MapHash[K, V]
  given [A] as Hash[SortedSet[A]] given Hash[A] = io.circe.cats.kernel.instances.SortedSetHash[A]
  given [K, V] as Hash[SortedMap[K, V]] given Hash[K], Order[K], Hash[V] = io.circe.cats.kernel.instances.SortedMapHash[K, V]
  given [A, B] as Hash[Either[A, B]] given Hash[A], Hash[B] = io.circe.cats.kernel.instances.EitherHash[A, B]
}

private trait EqInstances {
  given [A] as Eq[Option[A]] given Eq[A] = io.circe.cats.kernel.instances.OptionEq[A]
  given [A] as Eq[List[A]] given Eq[A] = io.circe.cats.kernel.instances.ListEq[A]
  given [A] as Eq[Vector[A]] given Eq[A] = io.circe.cats.kernel.instances.VectorEq[A]
  given [A] as Eq[Stream[A]] given Eq[A] = io.circe.cats.kernel.instances.StreamEq[A]
  given [A] as Eq[Queue[A]] given Eq[A] = io.circe.cats.kernel.instances.QueueEq[A]
  given [K, V] as Eq[Map[K, V]] given Eq[V] = io.circe.cats.kernel.instances.MapEq[K, V]
  given [K, V] as Eq[SortedMap[K, V]] given Order[K], Eq[V] = io.circe.cats.kernel.instances.SortedMapEq[K, V]
  given [A, B] as Eq[Either[A, B]] given Eq[A], Eq[B] = io.circe.cats.kernel.instances.EitherEq[A, B] 
}
