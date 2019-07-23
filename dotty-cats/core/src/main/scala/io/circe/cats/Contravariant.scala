package io.circe.cats

import io.circe.cats.kernel.Hash

/**
 * Must obey the laws defined in cats.laws.ContravariantLaws.
 */
trait Contravariant[F[_]] extends Invariant[F] { self =>
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
  override def imap[A, B](fa: F[A])(f: A => B)(fi: B => A): F[B] = contramap(fa)(fi)

  /**
   * Lifts natural subtyping contravariance of contravariant Functors.
   * could be implemented as contramap(identity), but the Functor laws say this is equivalent
   */
  def narrow[A, B <: A](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]

  def liftContravariant[A, B](f: A => B): F[B] => F[A] = contramap(_: F[B])(f)

  def compose[G[_]] given Contravariant[G]: Functor[[x] =>> F[G[x]]] =
    new ComposedContravariant[F, G] {
      val F = self
      val G = the[Contravariant[G]]
    }

  override def composeFunctor[G[_]] given Functor[G]: Contravariant[[x] =>> F[G[x]]] =
    new ComposedContravariantCovariant[F, G] {
      val F = self
      val G = the[Functor[G]]
    }
}

object Contravariant {
  def apply[F[_]] given (F: Contravariant[F]): Contravariant[F] = F

  given [F[_]] as Contravariant[F] given (F: ContravariantSemigroupal[F]) = F

  given as Contravariant[Hash] {
      /**
       * Derive a `Hash` for `B` given an `Hash[A]` and a function `B => A`.
       */
    def contramap[A, B](A: Hash[A])(f: B => A): Hash[B] = Hash.by(f) given A
  }

  given [R] as Contravariant[[x] =>> x => R] {
    def contramap[A, B](fa: A => R)(f: B => A): B => R =
      fa.compose(f)
  }

  private[cats] trait Ops {
    given [F[_], A] given (F: Contravariant[F]) {
      def (fa: F[A]) contramap[B](f: B => A): F[B] = F.contramap(fa)(f)
    }
  }
}
