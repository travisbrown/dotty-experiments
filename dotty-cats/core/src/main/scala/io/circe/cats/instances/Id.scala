package io.circe.cats.instances

import io.circe.cats.{Apply, Bimonad, CommutativeMonad, Comonad, Distributive, Eval, Functor, Id, NonEmptyTraverse, Now} 
import io.circe.cats.kernel.{Monoid, Semigroup}
import scala.annotation.tailrec

private[cats] object IdInstance extends Bimonad[Id] with CommutativeMonad[Id] with Comonad[Id] with NonEmptyTraverse[Id] with Distributive[Id] {
  def pure[A](a: A): A = a
  def extract[A](a: A): A = a
  def flatMap[A, B](a: A)(f: A => B): B = f(a)
  def coflatMap[A, B](a: A)(f: A => B): B = f(a)
  @tailrec def tailRecM[A, B](a: A)(f: A => Either[A, B]): B = f(a) match {
    case Left(a1) => tailRecM(a1)(f)
    case Right(b) => b
  }
  override def distribute[F[_], A, B](fa: F[A])(f: A => B) given (F: Functor[F]): Id[F[B]] = F.map(fa)(f)
  override def map[A, B](fa: A)(f: A => B): B = f(fa)
  override def ap[A, B](ff: A => B)(fa: A): B = ff(fa)
  override def flatten[A](ffa: A): A = ffa
  override def map2[A, B, Z](fa: A, fb: B)(f: (A, B) => Z): Z = f(fa, fb)
  override def lift[A, B](f: A => B): A => B = f
  override def imap[A, B](fa: A)(f: A => B)(fi: B => A): B = f(fa)
  def foldLeft[A, B](a: A, b: B)(f: (B, A) => B) = f(b, a)
  def foldRight[A, B](a: A, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    f(a, lb)
  def nonEmptyTraverse[G[_], A, B](a: A)(f: A => G[B]) given Apply[G]: G[B] =
    f(a)
  override def foldMap[A, B](fa: Id[A])(f: A => B) given Monoid[B]: B = f(fa)
  override def reduce[A](fa: Id[A]) given (A: Semigroup[A]): A =
    fa
  def reduceLeftTo[A, B](fa: Id[A])(f: A => B)(g: (B, A) => B): B =
    f(fa)
  override def reduceLeft[A](fa: Id[A])(f: (A, A) => A): A =
    fa
  override def reduceLeftToOption[A, B](fa: Id[A])(f: A => B)(g: (B, A) => B): Option[B] =
    Some(f(fa))
  override def reduceRight[A](fa: Id[A])(f: (A, Eval[A]) => Eval[A]): Eval[A] =
    Now(fa)
  def reduceRightTo[A, B](fa: Id[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
    Now(f(fa))
  override def reduceRightToOption[A, B](fa: Id[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
    Now(Some(f(fa)))
  override def reduceMap[A, B](fa: Id[A])(f: A => B) given Semigroup[B]: B = f(fa)
  override def size[A](fa: Id[A]): Long = 1L
  override def get[A](fa: Id[A])(idx: Long): Option[A] =
    if (idx == 0L) Some(fa) else None
  override def isEmpty[A](fa: Id[A]): Boolean = false
}
