package io.circe.cats

import io.circe.cats.kernel.{CommutativeSemigroup, Eq, Monoid, Order, PartialOrder, Semigroup}

/**
 * [[Semigroupal]] captures the idea of composing independent effectful values.
 * It is of particular interest when taken together with [[Functor]] - where [[Functor]]
 * captures the idea of applying a unary pure function to an effectful value,
 * calling `product` with `map` allows one to apply a function of arbitrary arity to multiple
 * independent effectful values.
 *
 * That same idea is also manifested in the form of [[Apply]], and indeed [[Apply]] extends both
 * [[Semigroupal]] and [[Functor]] to illustrate this.
 */
trait Semigroupal[F[_]] extends Serializable {

  /**
   * Combine an `F[A]` and an `F[B]` into an `F[(A, B)]` that maintains the effects of both `fa` and `fb`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val noneInt: Option[Int] = None
   * scala> val some3: Option[Int] = Some(3)
   * scala> val noneString: Option[String] = None
   * scala> val someFoo: Option[String] = Some("foo")
   *
   * scala> Semigroupal[Option].product(noneInt, noneString)
   * res0: Option[(Int, String)] = None
   *
   * scala> Semigroupal[Option].product(noneInt, someFoo)
   * res1: Option[(Int, String)] = None
   *
   * scala> Semigroupal[Option].product(some3, noneString)
   * res2: Option[(Int, String)] = None
   *
   * scala> Semigroupal[Option].product(some3, someFoo)
   * res3: Option[(Int, String)] = Some((3,foo))
   * }}}
   */
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object Semigroupal extends SemigroupalArityFunctions {
   def apply[F[_]] given (F: Semigroupal[F]): Semigroupal[F] = F

  given as Semigroupal[Option] = io.circe.cats.instances.OptionInstance
  given as Semigroupal[List] = io.circe.cats.instances.ListInstance
  given as Semigroupal[Vector] = io.circe.cats.instances.VectorInstance
  given as Semigroupal[Stream] = io.circe.cats.instances.StreamInstance

  given [R] as Semigroupal[[x] =>> x => R] given (R: Monoid[R]) = the[ContravariantMonoidal[[x] =>> x => R]]
  given as Semigroupal[Eq] = the[ContravariantMonoidal[Eq]]
  given as Semigroupal[Equiv] = the[ContravariantMonoidal[Equiv]]
  given as Semigroupal[Order] = the[ContravariantMonoidal[Order]]
  given as Semigroupal[Ordering] = the[ContravariantMonoidal[Ordering]]
  given as Semigroupal[PartialOrder] = the[ContravariantMonoidal[PartialOrder]]
  given as Semigroupal[PartialOrdering] = the[ContravariantMonoidal[PartialOrdering]]

  given as Semigroupal[Semigroup] = the[InvariantMonoidal[Semigroup]]
  given as Semigroupal[CommutativeSemigroup] = the[InvariantMonoidal[CommutativeSemigroup]]

  private[cats] trait Ops {
    given [F[_], A] given (F: Semigroupal[F]) {
      def (fa: F[A]) product[B](fb: F[B]): F[(A, B)] = F.product(fa, fb)
    }
  }
}
