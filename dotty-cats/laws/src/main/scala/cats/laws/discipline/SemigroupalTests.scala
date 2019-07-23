package io.circe.cats.laws.discipline

import io.circe.cats.{Invariant, Semigroupal}
import io.circe.cats.kernel.Eq
import io.circe.cats.kernel.laws.IsEq
import io.circe.cats.laws.SemigroupalLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.typelevel.discipline.Laws

trait SemigroupalTests[F[_]] extends Laws {
  def laws: SemigroupalLaws[F]

  def semigroupal[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]], Eq[F[A]], Eq[F[(A, B, C)]], Isomorphisms[F]: RuleSet =
    new DefaultRuleSet(
      name = "semigroupal",
      parent = None,
      "semigroupal associativity" -> Prop.forAll(
        (fa: F[A], fb: F[B], fc: F[C]) => the[Isomorphisms[F]].associativity(laws.semigroupalAssociativity[A, B, C](fa, fb, fc))
      )
    )
}

object SemigroupalTests {
  def apply[F[_]] given Semigroupal[F]: SemigroupalTests[F] =
    new SemigroupalTests[F] { def laws: SemigroupalLaws[F] = SemigroupalLaws[F] }

  trait Isomorphisms[F[_]] {
    def associativity[A, B, C](fs: (F[(A, (B, C))], F[((A, B), C)])): IsEq[F[(A, B, C)]]
    def leftIdentity[A](fs: (F[(Unit, A)], F[A])): IsEq[F[A]]
    def rightIdentity[A](fs: (F[(A, Unit)], F[A])): IsEq[F[A]]
  }

  object Isomorphisms {
    given FromInvariant[F[_]] as Isomorphisms[F] given (F: Invariant[F]) {
      def associativity[A, B, C](fs: (F[(A, (B, C))], F[((A, B), C)])): IsEq[F[(A, B, C)]] =
        F.imap(fs._1) { case (a, (b, c))   => (a, b, c) } { case (a, b, c) => (a, (b, c)) } <->
          F.imap(fs._2) { case ((a, b), c) => (a, b, c) } { case (a, b, c) => ((a, b), c) }

      def leftIdentity[A](fs: (F[(Unit, A)], F[A])): IsEq[F[A]] =
        F.imap(fs._1) { case (_, a) => a } { a =>
          ((), a)
        } <-> fs._2

      def rightIdentity[A](fs: (F[(A, Unit)], F[A])): IsEq[F[A]] =
        F.imap(fs._1) { case (a, _) => a } { a =>
          (a, ())
        } <-> fs._2
    }
  }

}
