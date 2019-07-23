package io.circe.cats.instances

import io.circe.cats.{Defer, Distributive, Functor}
import scala.annotation.tailrec

private[cats] object Function0Instance extends Defer[Function0] with Distributive[Function0] {
  private[this] case class Deferred[A](fa: () => Function0[A]) extends Function0[A] {
    def apply() = {
      @tailrec
      def loop(f: () => Function0[A]): A =
        f() match {
          case Deferred(f) => loop(f)
          case next        => next()
        }
      loop(fa)
    }
  }

  def defer[A](fa: => Function0[A]): Function0[A] = {
    lazy val cachedFa = fa
    Deferred(() => cachedFa)
  }
  
  def distribute[F[_], A, B](fa: F[A])(f: A => Function0[B]) given (F: Functor[F]): Function0[F[B]] = { () =>
    F.map(fa)(a => f(a)())
  }

  def map[A, B](fa: Function0[A])(f: A => B): Function0[B] = () => f(fa())
}