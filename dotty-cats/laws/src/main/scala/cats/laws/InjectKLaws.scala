package io.circe.cats.laws

import io.circe.cats.InjectK
import io.circe.cats.kernel.laws.IsEq

trait InjectKLaws[F[_], G[_]] given (F: InjectK[F, G]) {
  def injectKRoundTripInj[A](fa: F[A]): IsEq[Option[F[A]]] =
    F.prj.compose(F.inj).apply(fa) <-> Some(fa)

  def injectKRoundTripPrj[A](ga: G[A]): IsEq[Option[G[A]]] =
    F.prj(ga) match {
      case Some(fa) => (Some(F.inj(fa)): Option[G[A]]) <-> Some(ga)
      case None     => (None: Option[G[A]]) <-> None
    }
}

object InjectKLaws {
  def apply[F[_], G[_]] given InjectK[F, G]: InjectKLaws[F, G] =
    new InjectKLaws[F, G] {}
}
