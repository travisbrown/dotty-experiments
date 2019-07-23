package io.circe.cats.laws.discipline

import io.circe.cats.{Distributive, Functor}
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.DistributiveLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait DistributiveTests[F[_]] extends FunctorTests[F] {
  def laws: DistributiveLaws[F]

  def distributive[A, B, C, X[_], Y[_]] given Functor[X], Distributive[Y], Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[X[A]], Arbitrary[Y[C]], Arbitrary[F[Y[A]]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[A], Eq[F[A]], Eq[F[B]], Eq[F[C]], Eq[F[Y[X[C]]]], Eq[F[Y[A]]], Eq[Y[F[B]]]: RuleSet =
    new RuleSet {
      def name: String = "distributive"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(functor[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "distributive distribute identity" -> Prop.forAll(laws.distributeIdentity[A, B]),
        "distributive identity" -> Prop.forAll(laws.cosequenceIdentity[A]),
        "distributive composition" -> Prop.forAll(laws.composition[A, B, C, X, Y]),
        "distributive double cosequence identity" -> Prop.forAll(laws.cosequenceTwiceIsId[A, Y])
      )
    }
}

object DistributiveTests {
  def apply[F[_]] given Distributive[F]: DistributiveTests[F] =
    new DistributiveTests[F] { def laws: DistributiveLaws[F] = DistributiveLaws[F] }
}
