package io.circe.cats

import scala.annotation.tailrec

trait Bimonad[F[_]] extends Monad[F] with Comonad[F]

object Bimonad {
  def apply[F[_]] given (F: Bimonad[F]): Bimonad[F] = F

  given as Bimonad[Function0] {
    def extract[A](x: () => A): A = x()

    def coflatMap[A, B](fa: () => A)(f: (() => A) => B): () => B =
      () => f(fa)

    def pure[A](x: A): () => A = () => x

    def flatMap[A, B](fa: () => A)(f: A => () => B): () => B =
      () => f(fa())()

    def tailRecM[A, B](a: A)(fn: A => () => Either[A, B]): () => B =
      () => {
        @tailrec
        def loop(thisA: A): B = fn(thisA)() match {
          case Right(b)    => b
          case Left(nextA) => loop(nextA)
        }
        loop(a)
      }
  }
}