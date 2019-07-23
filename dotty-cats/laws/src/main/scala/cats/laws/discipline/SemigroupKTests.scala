package io.circe.cats.laws.discipline

import io.circe.cats.SemigroupK
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.SemigroupKLaws
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait SemigroupKTests[F[_]] extends Laws {
  def laws: SemigroupKLaws[F]

  def semigroupK[A] given Arbitrary[A], Arbitrary[F[A]], Eq[F[A]]: RuleSet =
    new DefaultRuleSet("semigroupK", None, "semigroupK associative" -> Prop.forAll(laws.semigroupKAssociative[A]))
}

object SemigroupKTests {
  def apply[F[_]] given SemigroupK[F]: SemigroupKTests[F] =
    new SemigroupKTests[F] { def laws: SemigroupKLaws[F] = SemigroupKLaws[F] }
}
