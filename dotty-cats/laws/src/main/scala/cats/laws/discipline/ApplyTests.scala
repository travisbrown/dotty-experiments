package io.circe.cats.laws.discipline

import io.circe.cats.Apply
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.ApplyLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait ApplyTests[F[_]] extends FunctorTests[F] with SemigroupalTests[F] {
  def laws: ApplyLaws[F]

  def apply[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]], Arbitrary[F[A => B]], Arbitrary[F[B => C]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[F[A]], Eq[F[C]], Eq[F[(A, B, C)]], Isomorphisms[F]: RuleSet =
    new RuleSet {
      val name = "apply"
      val parents = Seq(functor[A, B, C], semigroupal[A, B, C])
      val bases = Seq.empty
      val props = Seq(
        "apply composition" -> Prop.forAll(laws.applyComposition[A, B, C]),
        "map2/product-map consistency" -> Prop.forAll(laws.map2ProductConsistency[A, B, C]),
        "map2/map2Eval consistency" -> Prop.forAll(laws.map2EvalConsistency[A, B, C]),
        "productR consistent map2" -> Prop.forAll(laws.productRConsistency[A, C]),
        "productL consistent map2" -> Prop.forAll(laws.productLConsistency[A, C])
      )
    }
}

object ApplyTests {
  def apply[F[_]] given Apply[F]: ApplyTests[F] =
    new ApplyTests[F] { def laws: ApplyLaws[F] = ApplyLaws[F] }
}
