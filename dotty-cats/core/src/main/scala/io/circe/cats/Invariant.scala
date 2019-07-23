package io.circe.cats

import io.circe.cats.kernel._

/**
 * Must obey the laws defined in cats.laws.InvariantLaws.
 */
trait Invariant[F[_]] extends Serializable { self =>

  /**
   * Transform an `F[A]` into an `F[B]` by providing a transformation from `A`
   * to `B` and one from `B` to `A`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import scala.concurrent.duration._
   * scala> val durSemigroup: Semigroup[FiniteDuration] =
   *      | Invariant[Semigroup].imap(Semigroup[Long])(Duration.fromNanos)(_.toNanos)
   * scala> durSemigroup.combine(2.seconds, 3.seconds)
   * res1: FiniteDuration = 5 seconds
   * }}}
   */
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]

  def compose[G[_]] given Invariant[G]: Invariant[[x] =>> F[G[x]]] =
    new ComposedInvariant[F, G] {
      val F = self
      val G = the[Invariant[G]]
    }

  def composeFunctor[G[_]] given Functor[G]: Invariant[[x] =>> F[G[x]]] =
    new ComposedInvariantCovariant[F, G] {
      val F = self
      val G = the[Functor[G]]
    }

  def composeContravariant[G[_]] given Contravariant[G]: Invariant[[x] =>> F[G[x]]] =
    new ComposedInvariantContravariant[F, G] {
      val F = self
      val G = the[Contravariant[G]]
    }
}

object Invariant {
  def apply[F[_]] given (F: Invariant[F]): Invariant[F] = F

  given [F[_]] as Invariant[F] given (F: Contravariant[F]) = F
  given [F[_]] as Invariant[F] given (F: Functor[F]) = F

  given as Invariant[Monoid] {
    def imap[A, B](fa: Monoid[A])(f: A => B)(g: B => A): Monoid[B] = new Monoid[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.toIterator.map(g)).map(f)
    }
  }

  given as Invariant[Band] {
    def imap[A, B](fa: Band[A])(f: A => B)(g: B => A): Band[B] = new Band[B] {
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.toIterator.map(g)).map(f)
    }
  }

  given as Invariant[Semilattice] {
    def imap[A, B](fa: Semilattice[A])(f: A => B)(g: B => A): Semilattice[B] = new Semilattice[B] {
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.toIterator.map(g)).map(f)
    }
  }

  given as Invariant[CommutativeMonoid] {
    def imap[A, B](fa: CommutativeMonoid[A])(f: A => B)(g: B => A): CommutativeMonoid[B] = new CommutativeMonoid[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.toIterator.map(g)).map(f)
    }
  }

  given as Invariant[BoundedSemilattice] {
    def imap[A, B](fa: BoundedSemilattice[A])(f: A => B)(g: B => A): BoundedSemilattice[B] = new BoundedSemilattice[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.toIterator.map(g)).map(f)
    }
  }

  given as Invariant[Group] {
    def imap[A, B](fa: Group[A])(f: A => B)(g: B => A): Group[B] = new Group[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      def inverse(b: B): B = f(fa.inverse(g(b)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.toIterator.map(g)).map(f)
    }
  }

  given as Invariant[CommutativeGroup] {
    def imap[A, B](fa: CommutativeGroup[A])(f: A => B)(g: B => A): CommutativeGroup[B] = new CommutativeGroup[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      def inverse(b: B): B = f(fa.inverse(g(b)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.toIterator.map(g)).map(f)
    }
  }

  private[cats] trait Ops {
    given [F[_], A] given (F: Invariant[F]) {
      def (fa: F[A]) imap[B](f: A => B)(g: B => A): F[B] = F.imap(fa)(f)(g)
    }
  }
}
