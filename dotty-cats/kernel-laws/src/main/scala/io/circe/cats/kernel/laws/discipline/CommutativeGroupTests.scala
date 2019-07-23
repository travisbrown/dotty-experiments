package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.{ CommutativeGroup, Eq }
import io.circe.cats.kernel.laws.CommutativeGroupLaws
import org.scalacheck.{ Arbitrary, Prop }

trait CommutativeGroupTests[A] extends CommutativeMonoidTests[A] with GroupTests[A] {
  def laws: CommutativeGroupLaws[A]

  def commutativeGroup given Arbitrary[A], Eq[A]: RuleSet =
    new RuleSet {
      val name: String = "commutativeGroup"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(commutativeMonoid, group)
      val props: Seq[(String, Prop)] = Nil
    }

}

object CommutativeGroupTests {
  def apply[A] given CommutativeGroup[A]: CommutativeGroupTests[A] =
    new CommutativeGroupTests[A] { def laws: CommutativeGroupLaws[A] = CommutativeGroupLaws[A] }
}
