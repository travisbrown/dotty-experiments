package io.circe.cats.laws

import io.circe.cats.arrow.CommutativeArrow
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.compose._
import given io.circe.cats.syntax.strong._

/** Reference: "Causal Commutative Arrows", Journal of Functional Programming
 *  Figure 4.
 */
trait CommutativeArrowLaws[F[_, _]] given (F: CommutativeArrow[F]) extends ArrowLaws[F] {
  def arrowCommutative[A, B, C, D](f: F[A, B], g: F[C, D]): IsEq[F[(A, C), (B, D)]] =
    (F.first[A, B, C](f) >>> g.second[B]) <-> (F.second[C, D, A](g) >>> f.first[D])
}

object CommutativeArrowLaws {
  def apply[F[_, _]] given CommutativeArrow[F]: CommutativeArrowLaws[F] =
    new CommutativeArrowLaws[F] with ArrowLaws[F] with CategoryLaws[F] with StrongLaws[F] with ComposeLaws[F] with ProfunctorLaws[F]
}
