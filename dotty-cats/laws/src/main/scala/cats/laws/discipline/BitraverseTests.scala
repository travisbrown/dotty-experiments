package io.circe.cats.laws.discipline

import io.circe.cats.{Applicative, Bitraverse}
import io.circe.cats.kernel.{Eq, Monoid}
import io.circe.cats.laws.BitraverseLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait BitraverseTests[F[_, _]] extends BifoldableTests[F] with BifunctorTests[F] {
  def laws: BitraverseLaws[F]

  def bitraverse[G[_], A, B, C, D, E, H] given Applicative[G], Monoid[C],
    Arbitrary[F[A, B]], Arbitrary[F[A, D]], Arbitrary[G[C]], Arbitrary[G[D]], Arbitrary[G[E]], Arbitrary[G[H]],
    Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[E], Arbitrary[H],
    Cogen[A], Cogen[B], Cogen[C], Cogen[D], Cogen[E],
    Eq[F[A, B]], Eq[F[A, D]], Eq[F[A, H]], Eq[F[C, D]], Eq[F[C, H]], Eq[G[G[F[E, H]]]], Eq[C]: RuleSet =
    new RuleSet {
      val name = "bitraverse"
      val parents = Seq(bifoldable[A, B, C], bifunctor[A, B, C, D, E, H])
      val bases = Seq.empty
      val props = Seq(
        "bitraverse identity" -> Prop.forAll(laws.bitraverseIdentity[A, B]),
        "bitraverse composition" -> Prop.forAll(laws.bitraverseCompose[G, A, B, C, D, E, H])
      )
    }
}

object BitraverseTests {
  def apply[F[_, _]] given Bitraverse[F]: BitraverseTests[F] =
    new BitraverseTests[F] { def laws: BitraverseLaws[F] = BitraverseLaws[F] }
}
