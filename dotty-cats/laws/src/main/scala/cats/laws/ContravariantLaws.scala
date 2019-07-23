package io.circe.cats.laws

import io.circe.cats.Contravariant
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.contravariant._

/**
 * Laws that must be obeyed by any `cats.Contravariant`.
 */
trait ContravariantLaws[F[_]] given Contravariant[F] extends InvariantLaws[F] {
  def contravariantIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.contramap(identity[A]) <-> fa

  def contravariantComposition[A, B, C](fa: F[A], f: B => A, g: C => B): IsEq[F[C]] =
    fa.contramap(f).contramap(g) <-> fa.contramap(f.compose(g))
}

object ContravariantLaws {
  def apply[F[_]] given Contravariant[F]: ContravariantLaws[F] =
    new ContravariantLaws[F] with InvariantLaws[F] {}
}
