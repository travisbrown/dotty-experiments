package io.circe.cats.laws.discipline

import io.circe.cats.{CommutativeApplicative, UnorderedTraverse}
import io.circe.cats.kernel.{CommutativeMonoid, Eq}
import io.circe.cats.laws.UnorderedTraverseLaws
import io.circe.cats.laws.discipline.arbitrary._
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait UnorderedTraverseTests[F[_]] extends UnorderedFoldableTests[F] {
  def laws: UnorderedTraverseLaws[F]

  def unorderedTraverse[A, B, C, X[_], Y[_]] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    CommutativeApplicative[X], CommutativeApplicative[Y],
    Arbitrary[F[A]], Arbitrary[F[X[B]]], Arbitrary[X[B]], Arbitrary[Y[B]], Arbitrary[Y[C]],
    Cogen[A], Cogen[B],
    CommutativeMonoid[A], CommutativeMonoid[B],
    Eq[A], Eq[B], Eq[X[Y[F[C]]]], Eq[X[F[B]]], Eq[Y[F[B]]]: RuleSet = {
    given as Eq[(X[F[B]], Y[F[B]])] {
      def eqv(x: (X[F[B]], Y[F[B]]), y: (X[F[B]], Y[F[B]])): Boolean =
        the[Eq[X[F[B]]]].eqv(x._1, y._1) && the[Eq[Y[F[B]]]].eqv(x._2, y._2)
    }
    new DefaultRuleSet(
      name = "unorderedTraverse",
      parent = Some(unorderedFoldable[A, B]),
      "unordered traverse sequential composition" -> Prop.forAll(
        laws.unorderedTraverseSequentialComposition[A, B, C, X, Y]
      ),
      "unordered traverse parallel composition" -> Prop.forAll(laws.unorderedTraverseParallelComposition[A, B, X, Y]),
      "unordered traverse consistent with sequence" -> Prop.forAll(laws.unorderedSequenceConsistent[B, X])
    )
  }
}

object UnorderedTraverseTests {
  def apply[F[_]] given UnorderedTraverse[F]: UnorderedTraverseTests[F] =
    new UnorderedTraverseTests[F] { def laws: UnorderedTraverseLaws[F] = UnorderedTraverseLaws[F] }
}
