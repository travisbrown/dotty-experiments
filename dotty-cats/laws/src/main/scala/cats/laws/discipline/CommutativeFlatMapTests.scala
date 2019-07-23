package io.circe.cats.laws.discipline

import io.circe.cats.CommutativeFlatMap
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.CommutativeFlatMapLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait CommutativeFlatMapTests[F[_]] extends FlatMapTests[F] with CommutativeApplyTests[F] {
  def laws: CommutativeFlatMapLaws[F]

  def commutativeFlatMap[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Eq[A], Eq[B], Eq[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Arbitrary[F[A => B]], Arbitrary[F[B => C]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[F[A]], Eq[F[B]], Eq[F[C]],
    Eq[F[(A, B, C)]],
    Isomorphisms[F]: RuleSet =
    new RuleSet {
      def name: String = "commutative flatMap"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(flatMap[A, B, C], commutativeApply[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "flatmap commutativity" -> Prop.forAll(laws.flatmapCommutative[A, B, C])
      )
    }

}

object CommutativeFlatMapTests {
  def apply[F[_]] given CommutativeFlatMap[F]: CommutativeFlatMapTests[F] =
    new CommutativeFlatMapTests[F] {
      def laws: CommutativeFlatMapLaws[F] = CommutativeFlatMapLaws[F]
    }
}
