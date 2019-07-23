package io.circe.cats.laws

import io.circe.cats.Inject
import io.circe.cats.kernel.laws.IsEq

trait InjectLaws[A, B] given (A: Inject[A, B]) {
  def injectRoundTripInj(a: A): IsEq[Option[A]] =
    A.prj.compose(A.inj).apply(a) <-> Some(a)

  def injectRoundTripPrj(b: B): IsEq[Option[B]] =
    A.prj(b) match {
      case Some(a) => (Some(A.inj(a)): Option[B]) <-> Some(b)
      case None    => (None: Option[B]) <-> None
    }
}

object InjectLaws {
  def apply[A, B] given Inject[A, B]: InjectLaws[A, B] =
    new InjectLaws[A, B] {}
}
