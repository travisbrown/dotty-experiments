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

  given as Bitraverse[Either] {
    def bitraverse[G[_], A, B, C, D](fab: Either[A, B])(f: A => G[C],
                                                          g: B => G[D]) given (G: Applicative[G]): G[Either[C, D]] =
      fab match {
        case Left(a)  => G.map(f(a))(Left(_))
        case Right(b) => G.map(g(b))(Right(_))
      }

    def bifoldLeft[A, B, C](fab: Either[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      fab match {
        case Left(a)  => f(c, a)
        case Right(b) => g(c, b)
      }

    def bifoldRight[A, B, C](fab: Either[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C],
                                                              g: (B, Eval[C]) => Eval[C]): Eval[C] =
      fab match {
        case Left(a)  => f(a, c)
        case Right(b) => g(b, c)
      }
  }

  given as Bitraverse[Tuple2] {
    def bitraverse[G[_], A, B, C, D](fab: (A, B))(f: A => G[C], g: B => G[D]) given (G: Applicative[G]): G[(C, D)] =
      G.tuple2(f(fab._1), g(fab._2))
  
    def bifoldLeft[A, B, C](fab: (A, B), c: C)(f: (C, A) => C, g: (C, B) => C): C =
      g(f(c, fab._1), fab._2)
  
    def bifoldRight[A, B, C](fab: (A, B), c: Eval[C])(f: (A, Eval[C]) => Eval[C],
                                                      g: (B, Eval[C]) => Eval[C]): Eval[C] =
      g(fab._2, f(fab._1, c))
  }

  private[cats] trait Ops {    
    given [F[_, _], A, B] given (F: Bifoldable[F]) {
      def (fab: F[A, B]) bifoldLeft[C](c: C)(f: (C, A) => C, g: (C, B) => C): C = F.bifoldLeft(fab, c)(f, g)
      def (fab: F[A, B]) bifoldRight[C](c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] = F.bifoldRight(fab, c)(f, g)
      def (fab: F[A, B]) bifoldMap[C](f: A => C, g: B => C) given Monoid[C]: C = F.bifoldMap(fab)(f, g)
    }
  }
}