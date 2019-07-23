package io.circe.cats.laws.discipline

import io.circe.cats.NonEmptyParallel
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.NonEmptyParallelLaws
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait NonEmptyParallelTests[M[_], F[_]] extends Laws {
  def laws: NonEmptyParallelLaws[M, F]

  def nonEmptyParallel[A, B] given Arbitrary[A], Arbitrary[M[A]], Arbitrary[M[B]], Arbitrary[A => B], Eq[M[A]], Eq[M[B]], Arbitrary[F[A]], Eq[F[A]]: RuleSet =
    new DefaultRuleSet(
      "parallel",
      None,
      "parallel round trip" -> Prop.forAll((ma: M[A]) => laws.parallelRoundTrip(ma)),
      "sequential round trip" -> Prop.forAll((fa: F[A]) => laws.sequentialRoundTrip(fa)),
      "isomorphic functor" -> Prop.forAll((fa: F[A], f: A => B) => laws.isomorphicFunctor(fa, f))
    )
}

object NonEmptyParallelTests {
  def apply[M[_], F[_]] given NonEmptyParallel[M, F]: NonEmptyParallelTests[M, F] =
    new NonEmptyParallelTests[M, F] { val laws: NonEmptyParallelLaws[M, F] = NonEmptyParallelLaws[M, F] }
}
