package io.circe.cats.kernel

import io.circe.cats.kernel.instances.TupleCommutativeGroupInstances
import scala.collection.immutable.{BitSet, Queue, SortedMap, SortedSet}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.annotation.tailrec

/**
 * A semigroup is any set `A` with an associative operation (`combine`).
 */
trait Semigroup[@specialized(Int, Long, Float, Double) A] extends Any with Serializable {

  /**
   * Associative operation which combines two values.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   * scala> import cats.kernel.instances.int._
   * scala> import cats.kernel.instances.option._
   *
   * scala> Semigroup[String].combine("Hello ", "World!")
   * res0: String = Hello World!
   *
   * scala> Semigroup[Option[Int]].combine(None, Some(1))
   * res1: Option[Int] = Some(1)
   * }}}
   */
  def combine(x: A, y: A): A

  /**
   * Return `a` combined with itself `n` times.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.int._
   * scala> import cats.kernel.instances.string._
   *
   * scala> Semigroup[Int].combineN(1, 10)
   * res0: Int = 10
   *
   * scala> Semigroup[String].combineN("ha", 3)
   * res1: String = hahaha
   * }}}
   */
  def combineN(a: A, n: Int): A =
    if (n <= 0) throw new IllegalArgumentException("Repeated combining for semigroups must have n > 0")
    else repeatedCombineN(a, n)

  /**
   * Return `a` combined with itself more than once.
   */
  protected[this] def repeatedCombineN(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) combine(b, extra)
      else {
        val x = if ((k & 1) == 1) combine(b, extra) else extra
        loop(combine(b, b), k >>> 1, x)
      }
    if (n == 1) a else loop(a, n - 1, a)
  }

  /**
   * Given a sequence of `as`, combine them and return the total.
   *
   * If the sequence is empty, returns None. Otherwise, returns Some(total).
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Semigroup[String].combineAllOption(List("One ", "Two ", "Three"))
   * res0: Option[String] = Some(One Two Three)
   *
   * scala> Semigroup[String].combineAllOption(List.empty)
   * res1: Option[String] = None
   * }}}
   */
  def combineAllOption(as: TraversableOnce[A]): Option[A] =
    as.reduceOption(combine)
}

private[kernel] trait SemigroupFunctions[S[T] <: Semigroup[T]] {
  def combine[@specialized(Int, Long, Float, Double) A](x: A, y: A) given (A: S[A]): A =
    A.combine(x, y)

  def maybeCombine[@specialized(Int, Long, Float, Double) A](ox: Option[A], y: A) given (A: S[A]): A =
    ox match {
      case Some(x) => A.combine(x, y)
      case None    => y
    }

  def maybeCombine[@specialized(Int, Long, Float, Double) A](x: A, oy: Option[A]) given (A: S[A]): A =
    oy match {
      case Some(y) => A.combine(x, y)
      case None    => x
    }

  def combineN[@specialized(Int, Long, Float, Double) A](a: A, n: Int) given (A: S[A]): A =
    A.combineN(a, n)

  def combineAllOption[A](as: TraversableOnce[A]) given (A: S[A]): Option[A] =
    A.combineAllOption(as)
}

object Semigroup extends TupleCommutativeGroupInstances with SemilatticeInstances with SemigroupFunctions[Semigroup] {

  /**
   * Access a given `Semigroup[A]`.
   */
  def apply[A] given (A: Semigroup[A]): Semigroup[A] = A

  /**
   * Create a `Semigroup` instance from the given function.
   */
  def instance[A](cmb: (A, A) => A): Semigroup[A] = new Semigroup[A] {
    def combine(x: A, y: A): A = cmb(x, y)
  }

  given as (CommutativeGroup[Unit] & BoundedSemilattice[Unit]) = io.circe.cats.kernel.instances.UnitInstance
  given as CommutativeGroup[Byte] = io.circe.cats.kernel.instances.ByteInstance
  given as CommutativeGroup[Int] = io.circe.cats.kernel.instances.IntInstance
  given as CommutativeGroup[Short] = io.circe.cats.kernel.instances.ShortInstance
  given as CommutativeGroup[Long] = io.circe.cats.kernel.instances.LongInstance
  given as CommutativeGroup[BigInt] = io.circe.cats.kernel.instances.BigIntInstance
  given as CommutativeGroup[BigDecimal] = io.circe.cats.kernel.instances.BigDecimalInstance
  given as CommutativeGroup[Duration] = io.circe.cats.kernel.instances.DurationInstance
  given as CommutativeGroup[FiniteDuration] = io.circe.cats.kernel.instances.FiniteDurationInstance
  given as CommutativeGroup[Double] = io.circe.cats.kernel.instances.DoubleInstance
  given as CommutativeGroup[Float] = io.circe.cats.kernel.instances.FloatInstance

  given as Monoid[String] = io.circe.cats.kernel.instances.StringInstance
  given [A] as Monoid[Option[A]] given Semigroup[A] = io.circe.cats.kernel.instances.OptionMonoid[A]
  given [A] as Monoid[List[A]] = io.circe.cats.kernel.instances.ListMonoid[A]
  given [A] as Monoid[Vector[A]] = io.circe.cats.kernel.instances.VectorMonoid[A]
  given [A] as Monoid[Stream[A]] = io.circe.cats.kernel.instances.StreamMonoid[A]
  given [A] as Monoid[Queue[A]] = io.circe.cats.kernel.instances.QueueMonoid[A]

  given [A] as BoundedSemilattice[Set[A]] = io.circe.cats.kernel.instances.SetBoundedSemilattice[A]
  given as BoundedSemilattice[BitSet] = io.circe.cats.kernel.instances.BitSetInstance
  given [A] as BoundedSemilattice[SortedSet[A]] given Order[A] = io.circe.cats.kernel.instances.SortedSetBoundedSemilattice[A]

  given [A] as Group[() => A] given Group[A] = io.circe.cats.kernel.instances.Function0Group[A]
  given [A, B] as Group[A => B] given Group[B] = io.circe.cats.kernel.instances.Function1Group[A, B]

  given [K, V] as CommutativeMonoid[Map[K, V]] given Semigroup[V] = io.circe.cats.kernel.instances.MapCommutativeMonoid[K, V]
  given [K, V] as CommutativeMonoid[SortedMap[K, V]] given Order[K], CommutativeSemigroup[V] = io.circe.cats.kernel.instances.SortedMapCommutativeMonoid[K, V]
}

private trait SemilatticeInstances extends MonoidInstances {
  given [A] as Semilattice[() => A] given Semilattice[A] = io.circe.cats.kernel.instances.Function0Semilattice[A]
  given [A, B] as Semilattice[A => B] given Semilattice[B] = io.circe.cats.kernel.instances.Function1Semilattice[A, B]
}

private trait MonoidInstances extends CommutativeSemigroupInstances  {
  given [A] as Monoid[() => A] given Monoid[A] = io.circe.cats.kernel.instances.Function0Monoid[A]
  given [K, V] as Monoid[SortedMap[K, V]] given Order[K], Semigroup[V] = io.circe.cats.kernel.instances.SortedMapMonoid[K, V]
}

private trait CommutativeSemigroupInstances extends BandInstances {
  given [A] as CommutativeSemigroup[() => A] given CommutativeSemigroup[A] = io.circe.cats.kernel.instances.Function0CommutativeSemigroup[A]
}

private trait BandInstances extends SemigroupInstances {
  given [A] as Band[() => A] given Band[A] = io.circe.cats.kernel.instances.Function0Band[A]
}

private trait SemigroupInstances {
  given [A] as Semigroup[() => A] given Semigroup[A] = io.circe.cats.kernel.instances.Function0Semigroup[A]
  given [A, B] as Semigroup[A => B] given Semigroup[B] = io.circe.cats.kernel.instances.Function1Semigroup[A, B]
}
