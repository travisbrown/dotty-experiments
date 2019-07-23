package io.circe.cats.laws

import io.circe.cats.CommutativeApplicative

trait CommutativeApplicativeLaws[F[_]] given CommutativeApplicative[F] extends CommutativeApplyLaws[F] with ApplicativeLaws[F]

object CommutativeApplicativeLaws {
  def apply[F[_]] given CommutativeApplicative[F]: CommutativeApplicativeLaws[F] =
    new CommutativeApplicativeLaws[F] with CommutativeApplyLaws[F] with ApplicativeLaws[F] with ApplyLaws[F] with SemigroupalLaws[F] with FunctorLaws[F] with InvariantLaws[F]
}
