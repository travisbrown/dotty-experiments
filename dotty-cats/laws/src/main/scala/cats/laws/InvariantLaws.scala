package io.circe.cats.laws

import io.circe.cats.Invariant
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.invariant._

/**
 * Laws that must be obeyed by any `cats.Invariant`.
 */
trait InvariantLaws[F[_]] given Invariant[F] {
  def invariantIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.imap(identity[A])(identity[A]) <-> fa

  def invariantComposition[A, B, C](fa: F[A], f1: A => B, f2: B => A, g1: B => C, g2: C => B): IsEq[F[C]] =
    fa.imap(f1)(f2).imap(g1)(g2) <-> fa.imap(g1.compose(f1))(f2.compose(g2))
}

object InvariantLaws {
  def apply[F[_]] given Invariant[F]: InvariantLaws[F] =
    new InvariantLaws[F] {}
}
