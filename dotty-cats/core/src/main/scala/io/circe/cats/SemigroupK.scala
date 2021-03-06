package io.circe.cats

import io.circe.cats.arrow.Category
import io.circe.cats.data.AndThen
import io.circe.cats.kernel.Semigroup

/**
 * SemigroupK is a universal semigroup which operates on kinds.
 *
 * This type class is useful when its type parameter F[_] has a
 * structure that can be combined for any particular type. Thus,
 * SemigroupK is like a Semigroup for kinds (i.e. parametrized
 * types).
 *
 * A SemigroupK[F] can produce a Semigroup[F[A]] for any type A.
 *
 * Here's how to distinguish Semigroup and SemigroupK:
 *
 *  - Semigroup[A] allows two A values to be combined.
 *
 *  - SemigroupK[F] allows two F[A] values to be combined, for any A.
 *    The combination operation just depends on the structure of F,
 *    but not the structure of A.
 */
trait SemigroupK[F[_]] extends Serializable { self =>

  /**
   * Combine two F[A] values.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> SemigroupK[List].combineK(List(1, 2), List(3, 4))
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def combineK[A](x: F[A], y: F[A]): F[A]

  /**
   * Given a type A, create a concrete Semigroup[F[A]].
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val s: Semigroup[List[Int]] = SemigroupK[List].algebra[Int]
   * }}}
   */
  def algebra[A]: Semigroup[F[A]] =
    new Semigroup[F[A]] {
      def combine(x: F[A], y: F[A]): F[A] = self.combineK(x, y)
    }

  /**
   * "Compose" with a `G[_]` type to form a `SemigroupK` for `λ[α => F[G[α]]]`.
   * Note that this universally works for any `G`, because the "inner" structure
   * isn't considered when combining two instances.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> type ListOption[A] = List[Option[A]]
   * scala> val s: SemigroupK[ListOption] = SemigroupK[List].compose[Option]
   * scala> s.combineK(List(Some(1), None, Some(2)), List(Some(3), None))
   * res0: List[Option[Int]] = List(Some(1), None, Some(2), Some(3), None)
   * }}}
   */
  def compose[G[_]]: SemigroupK[[α] =>> F[G[α]]] =
    new ComposedSemigroupK[F, G] {
      val F = self
    }
}

object SemigroupK {
  def apply[F[_]] given (F: SemigroupK[F]): SemigroupK[F] = F

  given as MonoidK[Option] = io.circe.cats.instances.OptionInstance
  given as MonoidK[List] = io.circe.cats.instances.ListInstance
  given as MonoidK[Vector] = io.circe.cats.instances.VectorInstance
  given as MonoidK[Stream] = io.circe.cats.instances.StreamInstance
  given as MonoidK[Set] = io.circe.cats.instances.SetInstance

  given as MonoidK[Endo] {
    val category: Category[Function1] = the[Category[Function1]]

    override def empty[A]: Endo[A] = category.id

    override def combineK[A](x: Endo[A], y: Endo[A]): Endo[A] =
      AndThen(category.compose(x, y))
  }

  given [A] as SemigroupK[[x] =>> Either[A, x]] = io.circe.cats.instances.EitherInstance[A]
}