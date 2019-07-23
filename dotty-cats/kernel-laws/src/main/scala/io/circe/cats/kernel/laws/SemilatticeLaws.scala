package io.circe.cats.kernel.laws

import io.circe.cats.kernel.Semilattice

trait SemilatticeLaws[A] given Semilattice[A] extends CommutativeSemigroupLaws[A] with BandLaws[A]

object SemilatticeLaws {
  def apply[A] given (A: Semilattice[A]): SemilatticeLaws[A] =
    new SemilatticeLaws[A] with CommutativeSemigroupLaws[A] with BandLaws[A] with SemigroupLaws[A]
}
