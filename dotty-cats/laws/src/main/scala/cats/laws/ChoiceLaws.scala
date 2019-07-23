package io.circe.cats.laws

import io.circe.cats.arrow.Choice
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.choice._
import given io.circe.cats.syntax.compose._

/**
 * Laws that must be obeyed by any `cats.arrow.Choice`.
 */
trait ChoiceLaws[F[_, _]] given Choice[F] extends CategoryLaws[F] {
  def choiceCompositionDistributivity[A, B, C, D](fac: F[A, C], fbc: F[B, C], fcd: F[C, D]): IsEq[F[Either[A, B], D]] =
    ((fac ||| fbc) >>> fcd) <-> ((fac >>> fcd) ||| (fbc >>> fcd))
}

object ChoiceLaws {
  def apply[F[_, _]] given Choice[F]: ChoiceLaws[F] =
    new ChoiceLaws[F] with CategoryLaws[F] with ComposeLaws[F]
}
