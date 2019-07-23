package io.circe.cats

import io.circe.cats.kernel.Monoid

/**
 * A type class abstracting over types that give rise to two independent [[cats.Foldable]]s.
 */
trait Bifoldable[F[_, _]] { self =>

  /** Collapse the structure with a left-associative function */
  def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C

  /** Collapse the structure with a right-associative function */
  def bifoldRight[A, B, C](fab: F[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C]

  /** Collapse the structure by mapping each element to an element of a type that has a [[cats.Monoid]] */
  def bifoldMap[A, B, C](fab: F[A, B])(f: A => C, g: B => C) given (C: Monoid[C]): C =
    bifoldLeft(fab, C.empty)(
      (c: C, a: A) => C.combine(c, f(a)),
      (c: C, b: B) => C.combine(c, g(b))
    )

  def compose[G[_, _]] given Bifoldable[G]: Bifoldable[[x, y] =>> F[G[x, y], G[x, y]]] =
    new ComposedBifoldable[F, G] {
      val F = self
      val G = the[Bifoldable[G]]
    }
}

private[cats] trait ComposedBifoldable[F[_, _], G[_, _]] extends Bifoldable[[x, y] =>> F[G[x, y], G[x, y]]] {
  def F: Bifoldable[F]
  def G: Bifoldable[G]

  override def bifoldLeft[A, B, C](fab: F[G[A, B], G[A, B]], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F.bifoldLeft(fab, c)(
      (c: C, gab: G[A, B]) => G.bifoldLeft(gab, c)(f, g),
      (c: C, gab: G[A, B]) => G.bifoldLeft(gab, c)(f, g)
    )

  override def bifoldRight[A, B, C](fab: F[G[A, B], G[A, B]], c: Eval[C])(f: (A, Eval[C]) => Eval[C],
                                                                          g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F.bifoldRight(fab, c)(
      (gab: G[A, B], c: Eval[C]) => G.bifoldRight(gab, c)(f, g),
      (gab: G[A, B], c: Eval[C]) => G.bifoldRight(gab, c)(f, g)
    )
}

object Bifoldable {  
  def apply[F[_, _]] given (F: Bifoldable[F]): Bifoldable[F] = F

  given [F[_, _]] as Bifoldable[F] given (F: Bitraverse[F]) = F

  private[cats] trait Ops {    
    given [F[_, _], A, B] given (F: Bifoldable[F]) {
      def (fab: F[A, B]) bifoldLeft[C](c: C)(f: (C, A) => C, g: (C, B) => C): C = F.bifoldLeft(fab, c)(f, g)
      def (fab: F[A, B]) bifoldRight[C](c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] = F.bifoldRight(fab, c)(f, g)
      def (fab: F[A, B]) bifoldMap[C](f: A => C, g: B => C) given Monoid[C]: C = F.bifoldMap(fab)(f, g)
    }
  }
}