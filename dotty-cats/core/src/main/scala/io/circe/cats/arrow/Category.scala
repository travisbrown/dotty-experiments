package io.circe.cats.arrow

import io.circe.cats.MonoidK
import io.circe.cats.kernel.Monoid

/**
 * Must obey the laws defined in cats.laws.CategoryLaws.
 */
trait Category[F[_, _]] extends Compose[F] { self =>

  def id[A]: F[A, A]

  override def algebraK: MonoidK[[x] =>> F[x, x]] =
    new MonoidK[[x] =>> F[x, x]] {
      def empty[A]: F[A, A] = id
      def combineK[A](f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }

  override def algebra[A]: Monoid[F[A, A]] =
    new Monoid[F[A, A]] {
      def empty: F[A, A] = id
      def combine(f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }
}

object Category {  
  def apply[F[_, _]] given (F: Category[F]): Category[F] = F
}