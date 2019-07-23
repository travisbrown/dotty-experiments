package io.circe.cats.laws.discipline

import io.circe.cats.Bimonad
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.BimonadLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait BimonadTests[F[_]] extends MonadTests[F] with ComonadTests[F] {
  def laws: BimonadLaws[F]

  def bimonad[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Eq[A], Eq[B], Eq[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Arbitrary[F[F[A]]],
    Arbitrary[F[A => B]], Arbitrary[F[B => C]],
    Cogen[A], Cogen[B], Cogen[C],
    Cogen[F[A]], Cogen[F[B]],
    Eq[F[A]], Eq[F[B]], Eq[F[C]],
    Eq[F[F[A]]], Eq[F[F[F[A]]]],
    Eq[F[(A, B, C)]], Eq[F[Int]],
    Isomorphisms[F]: RuleSet =
    new RuleSet {
      def name: String = "bimonad"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monad[A, B, C], comonad[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "pure andThen extract = id" -> Prop.forAll(laws.pureExtractIsId[A]),
        "extract/flatMap entwining" -> Prop.forAll(laws.extractFlatMapEntwining[A]),
        "pure/coflatMap entwining" -> Prop.forAll(laws.pureCoflatMapEntwining[A])
      )
    }
}

object BimonadTests {
  def apply[F[_]] given Bimonad[F]: BimonadTests[F] =
    new BimonadTests[F] {
      def laws: BimonadLaws[F] = BimonadLaws[F]
    }
}
