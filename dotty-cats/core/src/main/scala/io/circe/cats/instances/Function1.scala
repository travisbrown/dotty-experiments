package io.circe.cats.instances

import io.circe.cats.{Defer, Distributive, Functor, Monad}
import io.circe.cats.arrow.{ArrowChoice, CommutativeArrow}
import scala.annotation.tailrec

private[cats] object Function1Instance extends ArrowChoice[Function1] with CommutativeArrow[Function1] {
  def choose[A, B, C, D](f: A => C)(g: B => D): Either[A, B] => Either[C, D] =
    _ match {
        case Left(a)  => Left(f(a))
        case Right(b) => Right(g(b))
    }

  def lift[A, B](f: A => B): A => B = f

  def first[A, B, C](fa: A => B): ((A, C)) => (B, C) = {
    case (a, c) => (fa(a), c)
  }

  override def split[A, B, C, D](f: A => B, g: C => D): ((A, C)) => (B, D) = {
    case (a, c) => (f(a), g(c))
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = f.compose(g)
}

private[cats] class Function1InstanceR[I] extends Distributive[[x] =>> I => x]
  with Defer[[x] =>> I => x]
  with Monad[[x] =>> I => x] {
  def pure[A](a: A): I => A = _ => a

  def flatMap[A, B](fa: I => A)(f: A => I => B): I => B =
    i => f(fa(i))(i)

  override def map[A, B](fa: I => A)(f: A => B): I => B = { i =>
    f(fa(i))
  }

  def tailRecM[A, B](a: A)(fn: A => I => Either[A, B]): I => B =
    (i: I) => {
      @tailrec
      def step(thisA: A): B = fn(thisA)(i) match {
        case Right(b)    => b
        case Left(nextA) => step(nextA)
      }
      step(a)
    }

  private[this] case class Deferred[B](fa: () => I => B) extends (I => B) {
    def apply(a: I) = {
      @tailrec
      def loop(f: () => I => B): B =
        f() match {
          case Deferred(f) => loop(f)
          case next        => next(a)
        }
      loop(fa)
    }
  }
    
  def defer[B](fa: => I => B): I => B = {
    lazy val cachedFa = fa
    Deferred(() => cachedFa)
  }

  def distribute[F[_], A, B](fa: F[A])(f: A => (I => B)) given (F: Functor[F]): I => F[B] = { i =>
    F.map(fa)(a => f(a)(i))
  }
}
