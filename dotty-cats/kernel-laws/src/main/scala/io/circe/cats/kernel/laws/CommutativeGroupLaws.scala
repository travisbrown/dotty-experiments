package io.circe.cats.kernel.laws

import io.circe.cats.kernel.CommutativeGroup

trait CommutativeGroupLaws[A] given CommutativeGroup[A] extends GroupLaws[A] with CommutativeMonoidLaws[A]

object CommutativeGroupLaws {
  def apply[A] given CommutativeGroup[A]: CommutativeGroupLaws[A] =
    new CommutativeGroupLaws[A] with GroupLaws[A] with CommutativeMonoidLaws[A] with CommutativeSemigroupLaws[A] with MonoidLaws[A] with SemigroupLaws[A]
}
