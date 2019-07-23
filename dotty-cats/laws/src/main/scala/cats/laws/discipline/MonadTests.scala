package io.circe.cats.laws.discipline

import io.circe.cats.Monad
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.MonadLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait MonadTests[F[_]] extends ApplicativeTests[F] with FlatMapTests[F] {
  def laws: MonadLaws[F]

  def monad[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Eq[A], Eq[B], Eq[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Arbitrary[F[A => B]], Arbitrary[F[B => C]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[F[A]], Eq[F[B]], Eq[F[C]],
    Eq[F[(A, B, C)]], Eq[F[Int]],
    Isomorphisms[F]: RuleSet =
    new RuleSet {
      def name: String = "monad"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C], flatMap[A, B, C])
      def props: Seq[(String, Prop)] =
        Seq(
          "monad left identity" -> Prop.forAll(laws.monadLeftIdentity[A, B]),
          "monad right identity" -> Prop.forAll(laws.monadRightIdentity[A]),
          "map flatMap coherence" -> Prop.forAll(laws.mapFlatMapCoherence[A, B]),
          "tailRecM stack safety" -> Prop.lzy(laws.tailRecMStackSafety)
        )
    }

  def stackUnsafeMonad[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Eq[A], Eq[B], Eq[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Arbitrary[F[A => B]], Arbitrary[F[B => C]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[F[A]], Eq[F[B]], Eq[F[C]],
    Eq[F[(A, B, C)]], Eq[F[Int]],
    Isomorphisms[F]: RuleSet =
    new RuleSet {
      def name: String = "monad (stack-unsafe)"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C], flatMap[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "monad left identity" -> Prop.forAll(laws.monadLeftIdentity[A, B]),
        "monad right identity" -> Prop.forAll(laws.monadRightIdentity[A]),
        "map flatMap coherence" -> Prop.forAll(laws.mapFlatMapCoherence[A, B])
      )
    }
}

object MonadTests {
  def apply[F[_]] given Monad[F]: MonadTests[F] =
    new MonadTests[F] {
      def laws: MonadLaws[F] = MonadLaws[F]
    }
}
