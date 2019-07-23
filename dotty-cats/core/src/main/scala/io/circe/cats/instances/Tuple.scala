package io.circe.cats.instances

import io.circe.cats.{Applicative, Comonad, Eval, Now, Reducible, Traverse}
import io.circe.cats.kernel.{Monoid, Semigroup}

private[cats] class Tuple2Instance[X] extends Traverse[[x] =>> (X, x)] with Comonad[[x] =>> (X, x)] with Reducible[[x] =>> (X, x)] {
  def traverse[G[_], A, B](fa: (X, A))(f: A => G[B]) given (G: Applicative[G]): G[(X, B)] =
    G.map(f(fa._2))((fa._1, _))

  def foldLeft[A, B](fa: (X, A), b: B)(f: (B, A) => B): B = f(b, fa._2)

  def foldRight[A, B](fa: (X, A), lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa._2, lb)

  override def map[A, B](fa: (X, A))(f: A => B): (X, B) = (fa._1, f(fa._2))

  def coflatMap[A, B](fa: (X, A))(f: ((X, A)) => B): (X, B) = (fa._1, f(fa))

  def extract[A](fa: (X, A)): A = fa._2

  override def coflatten[A](fa: (X, A)): (X, (X, A)) = (fa._1, fa)

  override def foldMap[A, B](fa: (X, A))(f: A => B) given Monoid[B]: B = f(fa._2)

  override def reduce[A](fa: (X, A)) given Semigroup[A]: A = fa._2

  def reduceLeftTo[A, B](fa: (X, A))(f: A => B)(g: (B, A) => B): B = f(fa._2)

  override def reduceLeft[A](fa: (X, A))(f: (A, A) => A): A = fa._2

  override def reduceLeftToOption[A, B](fa: (X, A))(f: A => B)(g: (B, A) => B): Option[B] =
    Some(f(fa._2))

  override def reduceRight[A](fa: (X, A))(f: (A, Eval[A]) => Eval[A]): Eval[A] =
    Now(fa._2)

  def reduceRightTo[A, B](fa: (X, A))(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
    Now(f(fa._2))

  override def reduceRightToOption[A, B](fa: (X, A))(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
    Now(Some(f(fa._2)))

  override def reduceMap[A, B](fa: (X, A))(f: A => B) given Semigroup[B]: B =
    f(fa._2)

  override def size[A](fa: (X, A)): Long = 1L

  override def get[A](fa: (X, A))(idx: Long): Option[A] =
    if (idx == 0L) Some(fa._2) else None

  override def exists[A](fa: (X, A))(p: A => Boolean): Boolean = p(fa._2)

  override def forall[A](fa: (X, A))(p: A => Boolean): Boolean = p(fa._2)

  override def isEmpty[A](fa: (X, A)): Boolean = false
}
