package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.{Eq, Semilattice}
import io.circe.cats.kernel.laws.SemilatticeLaws
import org.scalacheck.{Arbitrary, Prop}

trait SemilatticeTests[A] extends CommutativeSemigroupTests[A] with BandTests[A] {
  def laws: SemilatticeLaws[A]

  def semilattice given Arbitrary[A], Eq[A]: RuleSet =
    new RuleSet {
      val name: String = "semilattice"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(commutativeSemigroup, band)
      val props: Seq[(String, Prop)] = Nil
    }

}

object SemilatticeTests {
  def apply[A] given Semilattice[A]: SemilatticeTests[A] =
    new SemilatticeTests[A] { def laws: SemilatticeLaws[A] = SemilatticeLaws[A] }
}
