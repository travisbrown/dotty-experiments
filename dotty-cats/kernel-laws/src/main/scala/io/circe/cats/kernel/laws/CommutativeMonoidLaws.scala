package io.circe.cats.kernel.laws

import io.circe.cats.kernel.CommutativeMonoid

trait CommutativeMonoidLaws[A] given CommutativeMonoid[A] extends MonoidLaws[A] with CommutativeSemigroupLaws[A]

object CommutativeMonoidLaws {
  def apply[A] given CommutativeMonoid[A]: CommutativeMonoidLaws[A] =
    new CommutativeMonoidLaws[A] with MonoidLaws[A] with CommutativeSemigroupLaws[A] with SemigroupLaws[A]
}
