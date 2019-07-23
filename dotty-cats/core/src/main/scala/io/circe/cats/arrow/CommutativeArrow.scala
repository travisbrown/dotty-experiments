package io.circe.cats.arrow

/**
 * In a Commutative Arrow F[_, _], the split operation (or `***`) is commutative,
 * which means that there is non-interference between the effect of the paired arrows.
 *
 * Must obey the laws in CommutativeArrowLaws
 */
trait CommutativeArrow[F[_, _]] extends Arrow[F]

object CommutativeArrow {
  def apply[F[_, _]] given (F: CommutativeArrow[F]): CommutativeArrow[F] = F

  given as CommutativeArrow[Function1] = io.circe.cats.instances.Function1Instance
}
