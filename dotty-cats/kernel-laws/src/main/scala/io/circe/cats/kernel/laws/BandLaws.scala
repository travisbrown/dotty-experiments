package io.circe.cats.kernel.laws

import io.circe.cats.kernel.Band

trait BandLaws[A] given (A: Band[A]) extends SemigroupLaws[A] {
  def idempotence(x: A): IsEq[A] =
    A.combine(x, x) <-> x
}

object BandLaws {
  def apply[A] given Band[A]: BandLaws[A] =
    new BandLaws[A] with SemigroupLaws[A]
}
