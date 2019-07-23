package io.circe.cats.laws.discipline

import io.circe.cats.arrow.Arrow
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.ArrowLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait ArrowTests[F[_, _]] extends CategoryTests[F] with StrongTests[F] {
  def laws: ArrowLaws[F]

  def arrow[A, B, C, D, E, G] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[D], Arbitrary[E], Arbitrary[G],
    Arbitrary[F[A, B]], Arbitrary[F[B, C]], Arbitrary[F[A, C]], Arbitrary[F[C, D]], Arbitrary[F[D, E]], Arbitrary[F[E, G]],
    Cogen[A], Cogen[B], Cogen[C], Cogen[D], Cogen[E], Eq[F[A, A]], Eq[F[A, B]], Eq[F[A, C]], Eq[F[A, D]], Eq[F[A, G]],
    Eq[F[(A, C), B]],
    Eq[F[(A, C), (B, C)]], Eq[F[(A, C), (B, D)]], Eq[F[(A, D), (C, D)]], Eq[F[(A, D), (C, G)]],
    Eq[F[(D, A), (D, B)]], Eq[F[(C, A), (D, B)]],
    Eq[F[A, (B, C)]], Eq[F[(C, A), B]],
    Eq[F[((A, C), D), (B, (C, D))]], Eq[F[((A, C), D), ((B, C), D)]], Eq[F[(D, (C, A)), (D, (C, B))]]: RuleSet =
    new RuleSet {
      def name: String = "arrow"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(
        category[A, B, C, D],
        strong[A, B, C, D, E, G]
      )
      def props: Seq[(String, Prop)] = Seq(
        "arrow identity" -> laws.arrowIdentity[A],
        "arrow composition" -> Prop.forAll(laws.arrowComposition[A, B, C]),
        "arrow extension" -> Prop.forAll(laws.arrowExtension[A, B, C]),
        "arrow functor" -> Prop.forAll(laws.arrowFunctor[A, B, C, D]),
        "arrow exchange" -> Prop.forAll(laws.arrowExchange[A, B, C, D]),
        "arrow unit" -> Prop.forAll(laws.arrowUnit[A, B, C]),
        "arrow association" -> Prop.forAll(laws.arrowAssociation[A, B, C, D]),
        "split consistent with andThen" -> Prop.forAll(laws.splitConsistentWithAndThen[A, B, C, D]),
        "merge consistent with andThen" -> Prop.forAll(laws.mergeConsistentWithAndThen[A, B, C])
      )
    }
}

object ArrowTests {
  def apply[F[_, _]] given Arrow[F]: ArrowTests[F] =
    new ArrowTests[F] { def laws: ArrowLaws[F] = ArrowLaws[F] }
}
