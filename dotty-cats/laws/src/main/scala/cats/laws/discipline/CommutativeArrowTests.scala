package io.circe.cats.laws.discipline

import io.circe.cats.arrow.CommutativeArrow
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.CommutativeArrowLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait CommutativeArrowTests[F[_, _]] extends ArrowTests[F] {
  def laws: CommutativeArrowLaws[F]

  def commutativeArrow[A, B, C, D, E, G] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[D], Arbitrary[E], Arbitrary[G],
    Arbitrary[F[A, B]], Arbitrary[F[B, C]], Arbitrary[F[A, C]], Arbitrary[F[C, D]], Arbitrary[F[D, E]], Arbitrary[F[E, G]],
    Cogen[A], Cogen[B], Cogen[C], Cogen[D], Cogen[E],
    Eq[F[A, A]], Eq[F[A, B]], Eq[F[A, C]], Eq[F[A, D]], Eq[F[A, G]],
    Eq[F[(A, C), B]],
    Eq[F[(A, C), (B, C)]], Eq[F[(A, C), (B, D)]], Eq[F[(A, D), (C, D)]], Eq[F[(A, D), (C, G)]],
    Eq[F[(D, A), (D, B)]], Eq[F[(C, A), (D, B)]],
    Eq[F[A, (B, C)]], Eq[F[(C, A), B]],
    Eq[F[((A, C), D), (B, (C, D))]], Eq[F[((A, C), D), ((B, C), D)]], Eq[F[(D, (C, A)), (D, (C, B))]]: RuleSet =
    new DefaultRuleSet(name = "commutative arrow",
                       parent = Some(arrow[A, B, C, D, E, G]),
                       "arrow commutativity" -> Prop.forAll(laws.arrowCommutative[A, B, C, D]))
}

object CommutativeArrowTests {
  def apply[F[_, _]] given CommutativeArrow[F]: CommutativeArrowTests[F] =
    new CommutativeArrowTests[F] { def laws: CommutativeArrowLaws[F] = CommutativeArrowLaws[F] }
}
