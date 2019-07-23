package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.{ CommutativeMonoid, Eq }
import io.circe.cats.kernel.laws.CommutativeMonoidLaws
import org.scalacheck.{ Arbitrary, Prop }

trait CommutativeMonoidTests[A] extends CommutativeSemigroupTests[A] with MonoidTests[A] {
  def laws: CommutativeMonoidLaws[A]

  def commutativeMonoid given Arbitrary[A], Eq[A]: RuleSet =
    new RuleSet {
      val name: String = "commutativeMonoid"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(commutativeSemigroup, monoid)
      val props: Seq[(String, Prop)] = Nil
    }

}

object CommutativeMonoidTests {
  def apply[A] given CommutativeMonoid[A]: CommutativeMonoidTests[A] =
    new CommutativeMonoidTests[A] { def laws: CommutativeMonoidLaws[A] = CommutativeMonoidLaws[A] }
}
