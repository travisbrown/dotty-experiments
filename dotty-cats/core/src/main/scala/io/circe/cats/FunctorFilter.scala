package io.circe.cats

/**
 * `FunctorFilter[F]` allows you to `map` and filter out elements simultaneously.
 */
trait FunctorFilter[F[_]] extends Serializable {
  def functor: Functor[F]

  /**
   * A combined `map` and `filter`. Filtering is handled via `Option`
   * instead of `Boolean` such that the output type `B` can be different than
   * the input type `A`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val m: Map[Int, String] = Map(1 -> "one", 3 -> "three")
   * scala> val l: List[Int] = List(1, 2, 3, 4)
   * scala> def asString(i: Int): Option[String] = m.get(i)
   * scala> l.mapFilter(i => m.get(i))
   * res0: List[String] = List(one, three)
   * }}}
   */
  def mapFilter[A, B](fa: F[A])(f: A => Option[B]): F[B]

  /**
   * Similar to [[mapFilter]] but uses a partial function instead of a function
   * that returns an `Option`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l: List[Int] = List(1, 2, 3, 4)
   * scala> FunctorFilter[List].collect(l){
   *      |   case 1 => "one"
   *      |   case 3 => "three"
   *      | }
   * res0: List[String] = List(one, three)
   * }}}
   */
  def collect[A, B](fa: F[A])(f: PartialFunction[A, B]): F[B] =
    mapFilter(fa)(f.lift)

  /**
   * "Flatten" out a structure by collapsing `Option`s.
   * Equivalent to using `mapFilter` with `identity`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l: List[Option[Int]] = List(Some(1), None, Some(3), None)
   * scala> l.flattenOption
   * res0: List[Int] = List(1, 3)
   * }}}
   */
  def flattenOption[A](fa: F[Option[A]]): F[A] =
    mapFilter(fa)(identity)

  /**
   * Apply a filter to a structure such that the output structure contains all
   * `A` elements in the input structure that satisfy the predicate `f` but none
   * that don't.
   */
  def filter[A](fa: F[A])(f: A => Boolean): F[A] =
    mapFilter(fa)(a => if (f(a)) Some(a) else None)
}

object FunctorFilter {
  def apply[F[_]] given (F: FunctorFilter[F]): FunctorFilter[F] = F

  given [F[_]] as FunctorFilter[F] given (F: TraverseFilter[F]) = F

  private[cats] trait Ops {
    given [F[_], A] given (F: FunctorFilter[F]) {
      def (fa: F[A]) mapFilter[B](f: A => Option[B]): F[B] = F.mapFilter(fa)(f)
      def (fa: F[A]) collect[B](f: PartialFunction[A, B]): F[B] = F.collect(fa)(f)
      def (fa: F[A]) filter(f: A => Boolean): F[A] = F.filter(fa)(f)
    }
  }
}
