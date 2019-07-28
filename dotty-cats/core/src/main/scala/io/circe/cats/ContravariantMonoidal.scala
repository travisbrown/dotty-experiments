package io.circe.cats

import io.circe.cats.kernel.{Eq, Monoid, Order, PartialOrder}

/**
 * [[ContravariantMonoidal]] functors are functors that supply
 * a unit along the diagonal map for the `contramap2` operation.
 *
 * Must obey the laws defined in cats.laws.ContravariantMonoidalLaws.
 *
 * Based on ekmett's contravariant library:
 * https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html
 */
trait ContravariantMonoidal[F[_]] extends ContravariantSemigroupal[F] with InvariantMonoidal[F] {

  /**
   * `trivial` produces an instance of `F` for any type `A`
   * that is trivial with respect to `contramap2` along
   * the diagonal
   */
  def trivial[A]: F[A] = contramap(unit)(_ => ())
}

object ContravariantMonoidal {
  def apply[F[_]] given (F: ContravariantMonoidal[F]): ContravariantMonoidal[F] = F

  def monoid[F[_], A] given (F: ContravariantMonoidal[F]): Monoid[F[A]] =
    new ContravariantMonoidalMonoid[F, A]
}

private[cats] class ContravariantMonoidalMonoid[F[_], A] given (F: ContravariantMonoidal[F])
    extends ContravariantSemigroupalSemigroup[F, A]
    with Monoid[F[A]] {
  def empty: F[A] = F.trivial
}
