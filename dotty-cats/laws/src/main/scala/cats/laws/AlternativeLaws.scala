package io.circe.cats.laws

import io.circe.cats.Alternative
import io.circe.cats.kernel.Monoid
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.apply._
import given io.circe.cats.syntax.functor._
import given io.circe.cats.syntax.semigroup._

trait AlternativeLaws[F[_]] given (F: Alternative[F]) extends ApplicativeLaws[F] with MonoidKLaws[F] {
  given [A] as Monoid[F[A]] = F.algebra[A]

  def alternativeRightAbsorption[A, B](ff: F[A => B]): IsEq[F[B]] =
    (ff.ap(F.empty[A])) <-> F.empty[B]

  def alternativeLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => B): IsEq[F[B]] =
    ((fa |+| fa2).map(f)) <-> ((fa.map(f)) |+| (fa2.map(f)))

  def alternativeRightDistributivity[A, B](fa: F[A], ff: F[A => B], fg: F[A => B]): IsEq[F[B]] =
    ((ff |+| fg).ap(fa)) <-> ((ff.ap(fa)) |+| (fg.ap(fa)))

}

object AlternativeLaws {
  def apply[F[_]] given Alternative[F]: AlternativeLaws[F] =
    new AlternativeLaws[F] with ApplicativeLaws[F] with ApplyLaws[F] with MonoidKLaws[F] with SemigroupKLaws[F]  with SemigroupalLaws[F] with FunctorLaws[F] with InvariantLaws[F]
}
