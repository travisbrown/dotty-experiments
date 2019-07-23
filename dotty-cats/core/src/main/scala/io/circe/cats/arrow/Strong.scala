package io.circe.cats.arrow

/**
 * Must obey the laws defined in cats.laws.StrongLaws.
 */
trait Strong[F[_, _]] extends Profunctor[F] {

  /**
   * Create a new `F` that takes two inputs, but only modifies the first input
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.arrow.Strong
   * scala> val f: Int => Int = _ * 2
   * scala> val fab = Strong[Function1].first[Int,Int,Int](f)
   * scala> fab((2,3))
   * res0: (Int, Int) = (4,3)
   * }}}
   */
  def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)]

  /**
   * Create a new `F` that takes two inputs, but only modifies the second input
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.arrow.Strong
   * scala> val f: Int => Int = _ * 2
   * scala> val fab = Strong[Function1].second[Int,Int,Int](f)
   * scala> fab((2,3))
   * res0: (Int, Int) = (2,6)
   * }}}
   */
  def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)]
}

object Strong {
  def apply[F[_, _]] given (F: Strong[F]): Strong[F] = F

  given [F[_, _]] as Strong[F] given (F: Arrow[F]) = F

  private[cats] trait Ops {
    given [F[_, _], A, B] {
      def (fab: F[A, B]) first[C] given (F: Strong[F]): F[(A, C), (B, C)] = F.first(fab)
      def (fab: F[A, B]) second[C] given (F: Strong[F]): F[(C, A), (C, B)] = F.second(fab)
    }
  }
}