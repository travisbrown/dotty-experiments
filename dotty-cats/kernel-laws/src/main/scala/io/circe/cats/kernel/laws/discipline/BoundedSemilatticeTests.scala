package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.{Eq, BoundedSemilattice}
import io.circe.cats.kernel.laws.BoundedSemilatticeLaws
import org.scalacheck.{Arbitrary, Prop}

trait BoundedSemilatticeTests[A] extends CommutativeMonoidTests[A] with SemilatticeTests[A] {
  def laws: BoundedSemilatticeLaws[A]

  def boundedSemilattice given Arbitrary[A], Eq[A]: RuleSet =
    new RuleSet {
      val name: String = "boundedSemilattice"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(commutativeMonoid, semilattice)
      val props: Seq[(String, Prop)] = Nil
    }

}

object BoundedSemilatticeTests {
  def apply[A] given BoundedSemilattice[A]: BoundedSemilatticeTests[A] =
    new BoundedSemilatticeTests[A] { def laws: BoundedSemilatticeLaws[A] = BoundedSemilatticeLaws[A] }
}
