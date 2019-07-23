package io.circe.cats.kernel

import io.circe.cats.kernel.instances.TupleMonoidInstances
import scala.collection.immutable.{Queue, SortedMap}
import scala.specialized

/**
 * A monoid is a semigroup with an identity. A monoid is a specialization of a
 * semigroup, so its operation must be associative. Additionally,
 * `combine(x, empty) == combine(empty, x) == x`. For example, if we have `Monoid[String]`,
 * with `combine` as string concatenation, then `empty = ""`.
 */
trait Monoid[@specialized(Int, Long, Float, Double) A] extends Any with Semigroup[A] {

  /**
   * Return the identity element for this monoid.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.int._
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].empty
   * res0: String = ""
   *
   * scala> Monoid[Int].empty
   * res1: Int = 0
   * }}}
   */
  def empty: A

  /**
   * Tests if `a` is the identity.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].isEmpty("")
   * res0: Boolean = true
   *
   * scala> Monoid[String].isEmpty("something")
   * res1: Boolean = false
   * }}}
   */
  def isEmpty(a: A) given (A: Eq[A]): Boolean =
    A.eqv(a, empty)

  /**
   * Return `a` appended to itself `n` times.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].combineN("ha", 3)
   * res0: String = hahaha
   *
   * scala> Monoid[String].combineN("ha", 0)
   * res1: String = ""
   * }}}
   */
  override def combineN(a: A, n: Int): A =
    if (n < 0) throw new IllegalArgumentException("Repeated combining for monoids must have n >= 0")
    else if (n == 0) empty
    else repeatedCombineN(a, n)

  /**
   * Given a sequence of `as`, sum them using the monoid and return the total.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].combineAll(List("One ", "Two ", "Three"))
   * res0: String = One Two Three
   *
   * scala> Monoid[String].combineAll(List.empty)
   * res1: String = ""
   * }}}
   */
  def combineAll(as: TraversableOnce[A]): A =
    as.toIterator.foldLeft(empty)(combine)

  override def combineAllOption(as: TraversableOnce[A]): Option[A] =
    if (as.toIterator.isEmpty) None else Some(combineAll(as))
}

private[kernel] abstract class MonoidFunctions[M[T] <: Monoid[T]] extends SemigroupFunctions[M] {
  def empty[@specialized(Int, Long, Float, Double) A] given (A: M[A]): A =
    A.empty

  def isEmpty[@specialized(Int, Long, Float, Double) A](a: A) given M[A], Eq[A]: Boolean =
    the[M[A]].isEmpty(a)

  def combineAll[@specialized(Int, Long, Float, Double) A](as: TraversableOnce[A]) given (A: M[A]): A =
    A.combineAll(as)
}

object Monoid extends MonoidFunctions[Monoid] with TupleMonoidInstances {

  /**
   * Access a given `Monoid[A]`.
   */
  def apply[A] given (A: Monoid[A]): Monoid[A] = A

  /**
   * Create a `Monoid` instance from the given function and empty value.
   */
  def instance[A](emptyValue: A, cmb: (A, A) => A): Monoid[A] = new Monoid[A] {
    override val empty: A = emptyValue

    override def combine(x: A, y: A): A = cmb(x, y)
  }

  given [A] as Monoid[A] given (A: CommutativeMonoid[A]) = A

  given as Monoid[String] = io.circe.cats.kernel.instances.StringInstance
  given [A] as Monoid[Option[A]] given Semigroup[A] = io.circe.cats.kernel.instances.OptionMonoid[A]
  given [A] as Monoid[List[A]] = io.circe.cats.kernel.instances.ListMonoid[A]
  given [A] as Monoid[Vector[A]] = io.circe.cats.kernel.instances.VectorMonoid[A]
  given [A] as Monoid[Stream[A]] = io.circe.cats.kernel.instances.StreamMonoid[A]
  given [A] as Monoid[Queue[A]] = io.circe.cats.kernel.instances.QueueMonoid[A]

  given [A] as Monoid[() => A] given Monoid[A] = io.circe.cats.kernel.instances.Function0Monoid[A]
  given [K, V] as Monoid[SortedMap[K, V]] given Order[K], Semigroup[V] = io.circe.cats.kernel.instances.SortedMapMonoid[K, V]
}
