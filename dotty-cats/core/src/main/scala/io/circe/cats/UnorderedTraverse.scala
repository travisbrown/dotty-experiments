package io.circe.cats

/**
 * `UnorderedTraverse` is like a `Traverse` for unordered containers.
 */
trait UnorderedTraverse[F[_]] extends UnorderedFoldable[F] {
  def unorderedTraverse[G[_], A, B](fa: F[A])(f: A => G[B]) given CommutativeApplicative[G]: G[F[B]]

  def unorderedSequence[G[_], A](fga: F[G[A]]) given CommutativeApplicative[G]: G[F[A]] =
    unorderedTraverse(fga)(identity)
}

object UnorderedTraverse {
  def apply[F[_]] given (F: UnorderedTraverse[F]): UnorderedTraverse[F] = F

  private[cats] trait Ops {
    given [F[_], A] {
      def (fa: F[A]) unorderedTraverse[G[_], A, B](f: A => G[B])
        given (F: UnorderedTraverse[F], G: CommutativeApplicative[G]): G[F[B]] = F.unorderedTraverse(fa)(f)
    }
  }
}