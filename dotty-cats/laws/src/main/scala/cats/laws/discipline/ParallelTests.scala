package io.circe.cats.laws.discipline

import io.circe.cats.Parallel
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.ParallelLaws
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait ParallelTests[M[_], F[_]] extends NonEmptyParallelTests[M, F] {
  def laws: ParallelLaws[M, F]

  def parallel[A, B] given Arbitrary[A], Arbitrary[M[A]], Arbitrary[M[B]], Arbitrary[A => B], Eq[M[A]], Eq[M[B]], Arbitrary[F[A]], Eq[F[A]]: RuleSet =
    new DefaultRuleSet(
      "parallel",
      Some(nonEmptyParallel[A, B]),
      "isomorphic pure" -> Prop.forAll((a: A) => laws.isomorphicPure(a))
    )
}

object ParallelTests {
  def apply[M[_], F[_]] given Parallel[M, F]: ParallelTests[M, F] =
    new ParallelTests[M, F] { val laws: ParallelLaws[M, F] = ParallelLaws[M, F] }
}
