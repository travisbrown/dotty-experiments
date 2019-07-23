package io.circe.cats.kernel.laws

import io.circe.cats.kernel.BoundedSemilattice

trait BoundedSemilatticeLaws[A] given BoundedSemilattice[A] extends CommutativeMonoidLaws[A] with SemilatticeLaws[A]

object BoundedSemilatticeLaws {
  def apply[A] given (A: BoundedSemilattice[A]): BoundedSemilatticeLaws[A] =
    new BoundedSemilatticeLaws[A] with CommutativeMonoidLaws[A] with CommutativeSemigroupLaws[A] with MonoidLaws[A] with SemigroupLaws[A] with BandLaws[A] with SemilatticeLaws[A]
}
