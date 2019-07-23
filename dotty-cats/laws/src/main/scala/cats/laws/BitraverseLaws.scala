package io.circe.cats.laws

import io.circe.cats.{Applicative, Bitraverse, Id}
import io.circe.cats.data.Nested
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.bifoldable._

trait BitraverseLaws[F[_, _]] given (F: Bitraverse[F]) extends BifoldableLaws[F] with BifunctorLaws[F] {
  def bitraverseIdentity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    fab <-> F.bitraverse[Id, A, B, A, B](fab)(identity, identity)

  def bitraverseCompose[G[_], A, B, C, D, E, H](
    fab: F[A, B],
    f: A => G[C],
    g: B => G[D],
    h: C => G[E],
    i: D => G[H]
  ) given (G: Applicative[G]): IsEq[G[G[F[E, H]]]] = {
    val fg = F.bitraverse(fab)(f, g)
    val hi = G.map(fg)(f => F.bitraverse(f)(h, i))

    val c =
      F.bitraverse[[x] =>> Nested[G, G, x], A, B, E, H](fab)(
        a => Nested(G.map(f(a))(h)),
        b => Nested(G.map(g(b))(i))
      )

    hi <-> c.value
  }
}

object BitraverseLaws {
  def apply[F[_, _]] given Bitraverse[F]: BitraverseLaws[F] =
    new BitraverseLaws[F] with BifoldableLaws[F] with BifunctorLaws[F]
}
