package io.circe.cats

/**
 * A type class of types which give rise to two independent, covariant
 * functors.
 */
trait Bifunctor[F[_, _]] extends Serializable { self =>

  /**
   * The quintessential method of the Bifunctor trait, it applies a
   * function to each "side" of the bifunctor.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val x: (List[String], Int) = (List("foo", "bar"), 3)
   * scala> x.bimap(_.headOption, _.toLong + 1)
   * res0: (Option[String], Long) = (Some(foo),4)
   * }}}
   */
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  def rightFunctor[X]: Functor[[x] =>> F[X, x]] =
    new RightFunctor[F, X] { val F = self }

  def leftFunctor[X]: Functor[[x] =>> F[x, X]] =
    new LeftFunctor[F, X] { val F = self }

  // derived methods
  /**
   * apply a function to the "left" functor
   */
  def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = bimap(fab)(f, identity)

  /** The composition of two Bifunctors is itself a Bifunctor */
  def compose[G[_, _]] given Bifunctor[G]: Bifunctor[[x, y] =>> F[G[x, y], G[x, y]]] =
    new ComposedBifunctor[F, G] {
      val F = self
      val G = the[Bifunctor[G]]
    }

  /**
   * Widens A into a supertype AA.
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> sealed trait Foo
   * scala> case object Bar extends Foo
   * scala> val x1: Either[Bar.type, Int] = Either.left(Bar)
   * scala> val x2: Either[Foo, Int] = x1.leftWiden
   * }}}
   */
  def leftWiden[A, B, AA >: A](fab: F[A, B]): F[AA, B] = fab.asInstanceOf[F[AA, B]]
}

private trait ComposedBifunctor[F[_, _], G[_, _]] extends Bifunctor[[x, y] =>> F[G[x, y], G[x, y]]] {
  def F: Bifunctor[F]
  def G: Bifunctor[G]

  override def bimap[A, B, C, D](fab: F[G[A, B], G[A, B]])(f: A => C, g: B => D): F[G[C, D], G[C, D]] = {
    val innerBimap: G[A, B] => G[C, D] = gab => G.bimap(gab)(f, g)
    F.bimap(fab)(innerBimap, innerBimap)
  }
}

abstract private class LeftFunctor[F[_, _], X] extends Functor[[x] =>> F[x, X]] {
  def F: Bifunctor[F]

  override def map[A, C](fax: F[A, X])(f: A => C): F[C, X] =
    F.bimap(fax)(f, identity)
}

abstract private class RightFunctor[F[_, _], X] extends Functor[[x] =>> F[X, x]] {
  def F: Bifunctor[F]

  override def map[A, C](fxa: F[X, A])(f: A => C): F[X, C] =
    F.bimap(fxa)(identity, f)
}

object Bifunctor {  
  def apply[F[_, _]] given (F: Bifunctor[F]): Bifunctor[F] = F

  given as Bifunctor[Either] = the[Bitraverse[Either]]
  given as Bifunctor[Tuple2] = the[Bitraverse[Tuple2]]

  private[cats] trait Ops {    
    given [F[_, _], A, B] given (F: Bifunctor[F]) {
      def (fab: F[A, B]) bimap[C, D](f: A => C, g: B => D): F[C, D] = F.bimap(fab)(f, g)
      def (fab: F[A, B]) leftMap[C](f: A => C): F[C, B] = F.leftMap(fab)(f)
      def (fab: F[A, B]) leftWiden[AA >: A]: F[AA, B] = F.leftWiden(fab)
    }
  }
}