package io.circe.cats

trait Distributive[F[_]] extends Functor[F] { self =>

  /**
   * Given a function which returns a distributive `F`, apply that value across the structure G.
   */
  def distribute[G[_], A, B](ga: G[A])(f: A => F[B]) given Functor[G]: F[G[B]]

  /**
   * Given a Functor G which wraps some distributive F, distribute F across the G.
   */
  def cosequence[G[_], A](ga: G[F[A]]) given Functor[G]: F[G[A]] = distribute(ga)(identity)

  // Distributive composes
  def compose[G[_]] given Distributive[G]: Distributive[[x] =>> F[G[x]]] =
    new ComposedDistributive[F, G] {
      def F = self
      def G = the[Distributive[G]]
    }
}

object Distributive {
  def apply[F[_]] given (F: Distributive[F]): Distributive[F] = F

  given as Distributive[Id] = io.circe.cats.instances.IdInstance
  given as Distributive[Function0] = io.circe.cats.instances.Function0Instance
  given [I] as Distributive[[x] =>> I => x] = io.circe.cats.instances.Function1InstanceR[I]

  private[cats] trait Ops {
    given [G[_], A] {
      def (ga: G[A]) distribute[F[_], B](f: A => F[B]) given (G: Functor[G], F: Distributive[F]): F[G[B]] = F.distribute(ga)(f)
    }

    given [G[_], F[_], A] given (G: Functor[G], F: Distributive[F]) {
      def (gfa: G[F[A]]) cosequence: F[G[A]] = F.cosequence(gfa)
    }
  }
}
