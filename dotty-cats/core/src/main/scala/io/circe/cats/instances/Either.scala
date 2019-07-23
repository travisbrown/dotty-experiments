package io.circe.cats.instances

import io.circe.cats.{Applicative, Bitraverse, Eval, Monad, MonadError, Now, SemigroupK, Show, Traverse}
import io.circe.cats.kernel.Monoid
import scala.annotation.tailrec

private[cats] class EitherInstance[A] extends MonadError[[a] =>> Either[A, a], A] with Traverse[[a] =>> Either[A, a]] with SemigroupK[[a] =>> Either[A, a]] {
  def pure[B](b: B): Either[A, B] = Right(b)

  def flatMap[B, C](fa: Either[A, B])(f: B => Either[A, C]): Either[A, C] =
    fa.flatMap(f)

  def handleErrorWith[B](fea: Either[A, B])(f: A => Either[A, B]): Either[A, B] =
    fea match {
      case Left(e)      => f(e)
      case r @ Right(_) => r
    }

  def raiseError[B](e: A): Either[A, B] = Left(e)

  override def map[B, C](fa: Either[A, B])(f: B => C): Either[A, C] =
    fa.map(f)

  @tailrec
  final def tailRecM[B, C](b: B)(f: B => Either[A, Either[B, C]]): Either[A, C] =
    f(b) match {
      case left @ Left(_) =>
        left.asInstanceOf[Either[A, C]]
      case Right(e) =>
        e match {
          case Left(b1)         => tailRecM(b1)(f)
          case right @ Right(_) => right.asInstanceOf[Either[A, C]]
        }
    }

  override def map2Eval[B, C, Z](fb: Either[A, B], fc: Eval[Either[A, C]])(f: (B, C) => Z): Eval[Either[A, Z]] =
    fb match {
      case l @ Left(_) => Now(l.asInstanceOf[Either[A, Z]])
      case Right(b)    => fc.map(_.map(f(b, _)))
    }

  def traverse[F[_], B, C](fa: Either[A, B])(f: B => F[C])(implicit F: Applicative[F]): F[Either[A, C]] =
    fa match {
      case left @ Left(_) => F.pure(left.asInstanceOf[Either[A, C]])
      case Right(b)       => F.map(f(b))(Right(_))
    }

  def foldLeft[B, C](fa: Either[A, B], c: C)(f: (C, B) => C): C =
    fa match {
      case Left(_)  => c
      case Right(b) => f(c, b)
    }

  def foldRight[B, C](fa: Either[A, B], lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
    fa match {
      case Left(_)  => lc
      case Right(b) => f(b, lc)
    }

  override def attempt[B](fab: Either[A, B]): Either[A, Either[A, B]] =
    Right(fab)

  override def recover[B](fab: Either[A, B])(pf: PartialFunction[A, B]): Either[A, B] =
    fab match {
      case Left(a) if pf.isDefinedAt(a) => Right(pf(a))
      case Right(_) => fab
    }

  override def recoverWith[B](fab: Either[A, B])(pf: PartialFunction[A, Either[A, B]]): Either[A, B] =
    fab match {
      case Left(a) if pf.isDefinedAt(a) => pf(a)
      case Right(_) => fab
    }

  override def fromEither[B](fab: Either[A, B]): Either[A, B] =
    fab

  override def ensure[B](fab: Either[A, B])(error: => A)(predicate: B => Boolean): Either[A, B] =
    fab match {
      case Left(_) => fab
      case Right(b) => if (predicate(b)) fab else Left(error)
    }

  override def ensureOr[B](fab: Either[A, B])(error: B => A)(predicate: B => Boolean): Either[A, B] =
    fab match {
      case Left(_) => fab
      case Right(b) => if (predicate(b)) fab else Left(error(b))
    }

  override def reduceLeftToOption[B, C](fab: Either[A, B])(f: B => C)(g: (C, B) => C): Option[C] =
    fab.map(f).toOption

  override def reduceRightToOption[B, C](
    fab: Either[A, B]
  )(f: B => C)(g: (B, Eval[C]) => Eval[C]): Eval[Option[C]] =
    Now(fab.map(f).toOption)

  override def reduceLeftOption[B](fab: Either[A, B])(f: (B, B) => B): Option[B] =
    fab.toOption

  override def reduceRightOption[B](fab: Either[A, B])(f: (B, Eval[B]) => Eval[B]): Eval[Option[B]] =
    Now(fab.toOption)

  override def size[B](fab: Either[A, B]): Long =
    fab.fold(_ => 0L, _ => 1L)

  override def get[B](fab: Either[A, B])(idx: Long): Option[B] =
    if (idx == 0L) fab.fold(_ => None, Some(_)) else None

  override def foldMap[B, C](fab: Either[A, B])(f: B => C)(implicit C: Monoid[C]): C =
    fab.fold(_ => C.empty, f)

  override def find[B](fab: Either[A, B])(f: B => Boolean): Option[B] =
    fab.fold(_ => None, r => if (f(r)) Some(r) else None)

  override def exists[B](fab: Either[A, B])(p: B => Boolean): Boolean =
    fab.exists(p)

  override def forall[B](fab: Either[A, B])(p: B => Boolean): Boolean =
    fab.forall(p)

  override def toList[B](fab: Either[A, B]): List[B] =
    fab.fold(_ => Nil, _ :: Nil)

  override def isEmpty[B](fab: Either[A, B]): Boolean =
    fab.isLeft

  def combineK[B](x: Either[A, B], y: Either[A, B]): Either[A, B] = x match {
    case Left(_)  => y
    case Right(_) => x
  }
}
