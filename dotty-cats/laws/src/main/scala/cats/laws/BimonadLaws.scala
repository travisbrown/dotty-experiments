package io.circe.cats.laws

import io.circe.cats.Bimonad
import io.circe.cats.kernel.laws.IsEq

/**
 * Laws that must be obeyed by any `Bimonad`.
 *
 * For more information, see definition 4.1 from this paper:
 * http://arxiv.org/pdf/0710.1163v3.pdf
 */
trait BimonadLaws[F[_]] given (F: Bimonad[F]) extends MonadLaws[F] with ComonadLaws[F] {
  def pureExtractIsId[A](a: A): IsEq[A] =
    F.extract(F.pure(a)) <-> a

  def extractFlatMapEntwining[A](ffa: F[F[A]]): IsEq[A] =
    F.extract(F.flatten(ffa)) <-> F.extract(F.map(ffa)(F.extract))

  def pureCoflatMapEntwining[A](a: A): IsEq[F[F[A]]] =
    F.coflatten(F.pure(a)) <-> F.map(F.pure(a))(F.pure)
}

object BimonadLaws {
  def apply[F[_]] given Bimonad[F]: BimonadLaws[F] =
    new BimonadLaws[F] with MonadLaws[F] with FlatMapLaws[F] with ComonadLaws[F] with CoflatMapLaws[F] with SemigroupalLaws[F] with ApplicativeLaws[F] with ApplyLaws[F] with FunctorLaws[F] with InvariantLaws[F]
}
