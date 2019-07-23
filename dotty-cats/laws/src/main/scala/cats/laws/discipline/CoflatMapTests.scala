package io.circe.cats.laws.discipline

import io.circe.cats.CoflatMap
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.CoflatMapLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait CoflatMapTests[F[_]] extends FunctorTests[F] {
  def laws: CoflatMapLaws[F]

  def coflatMap[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[F[A]],
    Cogen[A], Cogen[B], Cogen[C],
    Cogen[F[A]], Cogen[F[B]],
    Eq[F[A]], Eq[F[B]], Eq[F[C]], Eq[F[F[A]]], Eq[F[F[F[A]]]]: RuleSet =
    new DefaultRuleSet(
      name = "coflatMap",
      parent = Some(functor[A, B, C]),
      "coflatMap associativity" -> Prop.forAll(laws.coflatMapAssociativity[A, B, C]),
      "coflatMap identity" -> Prop.forAll(laws.coflatMapIdentity[A, B]),
      "coflatten coherence" -> Prop.forAll(laws.coflattenCoherence[A, B]),
      "coflatten throughMap" -> Prop.forAll(laws.coflattenThroughMap[A])
    )
}

object CoflatMapTests {
  def apply[F[_]] given CoflatMap[F]: CoflatMapTests[F] =
    new CoflatMapTests[F] { def laws: CoflatMapLaws[F] = CoflatMapLaws[F] }
}
