package io.circe.cats.laws.discipline

import io.circe.cats.arrow.ArrowChoice
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.ArrowChoiceLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait ArrowChoiceTests[F[_, _]] extends ArrowTests[F] with ChoiceTests[F] {
  def laws: ArrowChoiceLaws[F]

  def arrowChoice[A, B, C, D, E, G] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[D], Arbitrary[E], Arbitrary[G],
    Arbitrary[F[A, B]], Arbitrary[F[B, C]], Arbitrary[F[A, C]], Arbitrary[F[A, D]], Arbitrary[F[C, D]], Arbitrary[F[D, E]], Arbitrary[F[E, G]],
    Cogen[A], Cogen[B], Cogen[C], Cogen[D], Cogen[E], Eq[F[A, A]], Eq[F[A, B]], Eq[F[A, C]], Eq[F[A, D]], Eq[F[A, G]],
    Eq[F[(A, C), B]], Eq[F[(A, C), (B, C)]], Eq[F[(A, C), (B, D)]], Eq[F[(A, D), (C, D)]], Eq[F[(A, D), (C, G)]], Eq[F[(D, A), (D, B)]], Eq[F[(C, A), (D, B)]],
    Eq[F[A, (B, C)]], Eq[F[(C, A), B]], Eq[F[((A, C), D), (B, (C, D))]], Eq[F[((A, C), D), ((B, C), D)]], Eq[F[(D, (C, A)), (D, (C, B))]],
    Eq[F[Either[A, B], D]], Eq[F[A, Either[B, C]]],
    Eq[F[Either[A, D], Either[C, D]]], Eq[F[Either[A, C], Either[B, C]]], Eq[F[Either[C, A], Either[C, B]]], Eq[F[Either[A, C], Either[B, D]]],
    Eq[F[Either[Either[A, B], C], Either[D, Either[B, C]]]]: RuleSet =
    new RuleSet {
      def name: String = "arrowChoice"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(
        arrow[A, B, C, D, E, G],
        choice[A, B, C, D]
      )
      def props: Seq[(String, Prop)] = Seq(
        "left and lift commute" -> Prop.forAll(laws.leftLiftCommute[A, B, C]),
        "left and compose commute" -> Prop.forAll(laws.leftComposeCommute[A, B, C, D]),
        "left and right consistent" -> Prop.forAll(laws.leftRightConsistent[A, B, C]),
        "left and then lift (Left.apply) commutes" -> Prop.forAll(laws.leftAndThenLiftedLeftApplyCommutes[A, B, C]),
        "left and then identity +++ _ commutes" -> Prop.forAll(laws.leftAndThenRightIdentityCommutes[A, B, C, D]),
        "left commutes with sum association" -> Prop.forAll(laws.leftTwiceCommutesWithSumAssociation[A, B, C, D])
      )
    }
}
object ArrowChoiceTests {
  def apply[F[_, _]] given ArrowChoice[F]: ArrowChoiceTests[F] =
    new ArrowChoiceTests[F] { def laws: ArrowChoiceLaws[F] = ArrowChoiceLaws[F] }
}
