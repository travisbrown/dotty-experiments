package io.circe.cats.laws.discipline

import io.circe.cats.arrow.Strong
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.StrongLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait StrongTests[F[_, _]] extends ProfunctorTests[F] {
  def laws: StrongLaws[F]

  def strong[A, B, C, D, E, G] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[D], Arbitrary[E], Arbitrary[G], Arbitrary[F[A, B]], Arbitrary[F[C, D]], Cogen[A], Cogen[B], Cogen[C], Cogen[D], Cogen[E], Eq[F[A, B]], Eq[F[A, D]], Eq[F[A, G]], Eq[F[(A, C), (B, C)]], Eq[F[(D, A), (D, B)]], Eq[F[(A, C), (B, D)]], Eq[F[(C, A), (D, B)]], Eq[F[(A, C), B]], Eq[F[(C, A), B]], Eq[F[((A, C), D), ((B, C), D)]], Eq[F[(D, (C, A)), (D, (C, B))]]: RuleSet =
    new DefaultRuleSet(
      name = "strong",
      parent = Some(profunctor[A, B, C, D, E, G]),
      "first is swapped second" -> Prop.forAll(laws.firstIsSwappedSecond[A, B, C]),
      "second is swapped first" -> Prop.forAll(laws.secondIsSwappedFirst[A, B, D]),
      "lmap equals first and then rmap" -> Prop.forAll(laws.lmapEqualsFirstAndThenRmap[A, B, C]),
      "lmap equals second and then rmap" -> Prop.forAll(laws.lmapEqualsSecondAndThenRmap[A, B, C]),
      "dinaturality of first" -> Prop.forAll(laws.dinaturalityFirst[A, B, C, D]),
      "dinaturality of second" -> Prop.forAll(laws.dinaturalitySecond[A, B, C, D]),
      "first first is dimap" -> Prop.forAll(laws.firstFirstIsDimap[A, B, C, D]),
      "second second is dimap" -> Prop.forAll(laws.secondSecondIsDimap[A, B, C, D])
    )
}

object StrongTests {
  def apply[F[_, _]] given Strong[F]: StrongTests[F] =
    new StrongTests[F] { def laws: StrongLaws[F] = StrongLaws[F] }
}
