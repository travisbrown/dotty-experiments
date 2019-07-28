package io.circe.cats

/**
 *  A type class abstracting over types that give rise to two independent [[cats.Traverse]]s.
 */
trait Bitraverse[F[_, _]] extends Bifoldable[F] with Bifunctor[F] { self =>

  /**
   * Traverse each side of the structure with the given functions.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption
   *
   * scala> ("1", "2").bitraverse(parseInt, parseInt)
   * res0: Option[(Int, Int)] = Some((1,2))
   *
   * scala> ("1", "two").bitraverse(parseInt, parseInt)
   * res1: Option[(Int, Int)] = None
   * }}}
   */
  def bitraverse[G[_], A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D]) given Applicative[G]: G[F[C, D]]

  /**
   * Invert the structure from F[G[A], G[B]] to G[F[A, B]].
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val rightSome: Either[Option[String], Option[Int]] = Either.right(Some(3))
   * scala> rightSome.bisequence
   * res0: Option[Either[String, Int]] = Some(Right(3))
   *
   * scala> val rightNone: Either[Option[String], Option[Int]] = Either.right(None)
   * scala> rightNone.bisequence
   * res1: Option[Either[String, Int]] = None
   *
   * scala> val leftSome: Either[Option[String], Option[Int]] = Either.left(Some("foo"))
   * scala> leftSome.bisequence
   * res2: Option[Either[String, Int]] = Some(Left(foo))
   *
   * scala> val leftNone: Either[Option[String], Option[Int]] = Either.left(None)
   * scala> leftNone.bisequence
   * res3: Option[Either[String, Int]] = None
   * }}}
   */
  def bisequence[G[_], A, B](fab: F[G[A], G[B]]) given Applicative[G]: G[F[A, B]] =
    bitraverse(fab)(identity, identity)

  /** If F and G are both [[cats.Bitraverse]] then so is their composition F[G[_, _], G[_, _]] */
  def compose[G[_, _]] given Bitraverse[G]: Bitraverse[[x, y] =>> F[G[x, y], G[x, y]]] =
    new ComposedBitraverse[F, G] {
      val F = self
      val G = the[Bitraverse[G]]
    }

  override def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] =
    bitraverse[Id, A, B, C, D](fab)(f, g)
}

private[cats] trait ComposedBitraverse[F[_, _], G[_, _]]
    extends Bitraverse[[x, y] =>> F[G[x, y], G[x, y]]]
    with ComposedBifoldable[F, G]
    with ComposedBifunctor[F, G] {
  def F: Bitraverse[F]
  def G: Bitraverse[G]

  override def bitraverse[H[_], A, B, C, D](fab: F[G[A, B], G[A, B]])(
    f: A => H[C],
    g: B => H[D]
  ) given Applicative[H]: H[F[G[C, D], G[C, D]]] =
    F.bitraverse(fab)(
      gab => G.bitraverse(gab)(f, g),
      gab => G.bitraverse(gab)(f, g)
    )
}

object Bitraverse {  
  def apply[F[_, _]] given (F: Bitraverse[F]): Bitraverse[F] = F

  private[cats] trait Ops {    
    given [F[_, _], A, B] given (F: Bitraverse[F]) {
      def (fab: F[A, B]) bitraverse[G[_],C, D](f: A => G[C], g: B => G[D]) given Applicative[G]: G[F[C, D]] = F.bitraverse(fab)(f, g)
    }
  }
}