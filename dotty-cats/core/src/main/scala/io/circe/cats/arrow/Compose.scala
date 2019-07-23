package io.circe.cats.arrow

import io.circe.cats.SemigroupK
import io.circe.cats.kernel.Semigroup

/**
 * Must obey the laws defined in cats.laws.ComposeLaws.
 *
 * Here's how you can use `>>>` and `<<<`
 * Example:
 * {{{
 * scala> import cats.implicits._
 * scala> val f : Int => Int = (_ + 1)
 * scala> val g : Int => Int = (_ * 100)
 * scala> (f >>> g)(3)
 * res0: Int = 400
 * scala> (f <<< g)(3)
 * res1: Int = 301
 * }}}
 */
trait Compose[F[_, _]] { self =>
  def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]

  def andThen[A, B, C](f: F[A, B], g: F[B, C]): F[A, C] =
    compose(g, f)

  def algebraK: SemigroupK[[x] =>> F[x, x]] =
    new SemigroupK[[x] =>> F[x, x]] {
      def combineK[A](f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }

  def algebra[A]: Semigroup[F[A, A]] =
    new Semigroup[F[A, A]] {
      def combine(f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }
}

object Compose {  
  def apply[F[_, _]] given (F: Compose[F]): Compose[F] = F

  given as Compose[Map] {
    /**
     * Compose two maps `g` and `f` by using the values in `f` as keys for `g`.
     * {{{
     * scala> import cats.arrow.Compose
     * scala> import cats.implicits._
     * scala> val first = Map(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "a")
     * scala> val second = Map("a" -> true, "b" -> false, "d" -> true)
     * scala> Compose[Map].compose(second, first)
     * res0: Map[Int, Boolean] = Map(1 -> true, 2 -> false, 4 -> true)
     * }}}
     */
    def compose[A, B, C](f: Map[B, C], g: Map[A, B]): Map[A, C] =
      g.foldLeft(Map.empty[A, C]) {
        case (acc, (key, value)) =>
          f.get(value) match {
            case Some(other) => acc + (key -> other)
            case _           => acc
          }
      }
  }

  private[cats] trait Ops {
    given ComposeOps[F[_, _], A, B] {
      def (f: F[A, B]) <<< [C] (g: F[C, A]) given (F: Compose[F]): F[C, B] = F.compose(f, g)
      def (f: F[A, B]) >>> [C] (g: F[B, C]) given (F: Compose[F]): F[A, C] = F.andThen(f, g)
      def (f: F[A, B]) compose[C](g: F[C, A]) given (F: Compose[F]): F[C, B] = F.compose(f, g)
      def (f: F[A, B]) andThen[C](g: F[B, C]) given (F: Compose[F]): F[A, C] = F.andThen(f, g)
    }
  }
}