package io.circe.cats.data

import io.circe.cats.{Alternative, Applicative, Apply, Bifoldable, Bifunctor, Bitraverse, Contravariant, Defer, Eval, FlatMap, Foldable, Functor, Monad, MonadError, SemigroupK, Show, Traverse, ~>}
import io.circe.cats.kernel.{Eq, Monoid, Order, PartialOrder, Semigroup}

/**
 * Transformer for `Either`, allowing the effect of an arbitrary type constructor `F` to be combined with the
 * fail-fast effect of `Either`.
 *
 * `EitherT[F, A, B]` wraps a value of type `F[Either[A, B]]`. An `F[C]` can be lifted in to `EitherT[F, A, C]` via `EitherT.right`,
 * and lifted in to a `EitherT[F, C, B]` via `EitherT.left`.
 */
final case class EitherT[F[_], A, B](value: F[Either[A, B]]) {
  def fold[C](fa: A => C, fb: B => C) given (F: Functor[F]): F[C] = F.map(value)(_.fold(fa, fb))

  def foldF[C](fa: A => F[C], fb: B => F[C]) given (F: FlatMap[F]): F[C] = F.flatMap(value)(_.fold(fa, fb))

  def isLeft given (F: Functor[F]): F[Boolean] = F.map(value)(_.isLeft)

  def isRight given (F: Functor[F]): F[Boolean] = F.map(value)(_.isRight)

  def swap given (F: Functor[F]): EitherT[F, B, A] = EitherT(F.map(value)(_.swap))

  def getOrElse[BB >: B](default: => BB) given (F: Functor[F]): F[BB] = F.map(value)(_.getOrElse(default))

  def getOrElseF[BB >: B](default: => F[BB]) given (F: Monad[F]): F[BB] =
    F.flatMap(value) {
      case Left(_)  => default
      case Right(b) => F.pure(b)
    }

  def orElse[C, BB >: B](default: => EitherT[F, C, BB]) given (F: Monad[F]): EitherT[F, C, BB] =
    EitherT(F.flatMap(value) {
      case Left(_)      => default.value
      case r @ Right(_) => F.pure(r.asInstanceOf[Either[C, BB]])
    })

  def recover(pf: PartialFunction[A, B]) given (F: Functor[F]): EitherT[F, A, B] =
    EitherT(F.map(value) {
      case Left(a) if pf.isDefinedAt(a) => Right(pf(a))
      case r @ Right(_) => r
    })

  def recoverWith(pf: PartialFunction[A, EitherT[F, A, B]]) given (F: Monad[F]): EitherT[F, A, B] =
    EitherT(F.flatMap(value) {
      case Left(a) if pf.isDefinedAt(a) => pf(a).value
      case other                        => F.pure(other)
    })

  /**
   * Inverse of `MonadError#attemptT`
   */
  def rethrowT given (F: MonadError[F, A]): F[B] =
    F.rethrow(value)

  def valueOr[BB >: B](f: A => BB) given (F: Functor[F]): F[BB] = fold(f, identity)

  def valueOrF[BB >: B](f: A => F[BB]) given (F: Monad[F]): F[BB] =
    F.flatMap(value) {
      case Left(a)  => f(a)
      case Right(b) => F.pure(b)
    }

  def forall(f: B => Boolean) given (F: Functor[F]): F[Boolean] = F.map(value)(_.forall(f))

  def exists(f: B => Boolean) given (F: Functor[F]): F[Boolean] = F.map(value)(_.exists(f))

  def ensure[AA >: A](onFailure: => AA)(f: B => Boolean) given (F: Functor[F]): EitherT[F, AA, B] =
    EitherT(F.map(value) {
      case l @ Left(_) => l
      case r @ Right(b) => if (f(b)) r else Left(onFailure)
    })

  def ensureOr[AA >: A](onFailure: B => AA)(f: B => Boolean) given (F: Functor[F]): EitherT[F, AA, B] =
    EitherT(F.map(value) {
      case l @ Left(_) => l
      case r @ Right(b) => if (f(b)) r else Left(onFailure(b))
    })

  def toOption given (F: Functor[F]): OptionT[F, B] = OptionT(F.map(value)(_.toOption))

  def to[G[_]] given (F: Functor[F], G: Alternative[G]): F[G[B]] =
    F.map(value) {
      case Left(_) => G.empty
      case Right(b) => G.pure(b)
    }

  def collectRight given (FA: Alternative[F], FM: Monad[F]): F[B] =
    FM.flatMap(value) {
      case Left(_) => FA.empty
      case Right(b) => FA.pure(b)      
    }

  def bimap[C, D](fa: A => C, fb: B => D) given (F: Functor[F]): EitherT[F, C, D] =
    EitherT(F.map(value) {
      case Left(a) => Left(fa(a))
      case Right(b) => Right(fb(b))
    })

  def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D]) given (F: Traverse[F],
                                                         G: Applicative[G]): G[EitherT[F, C, D]] =
    G.map(F.traverse(value)(axb => Bitraverse[Either].bitraverse(axb)(f, g)))(EitherT.apply)

  def biflatMap[C, D](fa: A => EitherT[F, C, D], fb: B => EitherT[F, C, D]) given (F: FlatMap[F]): EitherT[F, C, D] =
    EitherT(F.flatMap(value) {
      case Left(a)  => fa(a).value
      case Right(a) => fb(a).value
    })

  def applyAlt[D](ff: EitherT[F, A, B => D]) given (F: Apply[F]): EitherT[F, A, D] =
    EitherT[F, A, D](F.map2(this.value, ff.value)((xb, xbd) => Apply[[a] =>> Either[A, a]].ap(xbd)(xb)))

  def flatMap[AA >: A, D](f: B => EitherT[F, AA, D]) given (F: Monad[F]): EitherT[F, AA, D] =
    EitherT(F.flatMap(value) {
      case l @ Left(_) => F.pure(l.asInstanceOf[Either[AA, D]])
      case Right(b)    => f(b).value
    })

  def flatMapF[AA >: A, D](f: B => F[Either[AA, D]]) given (F: Monad[F]): EitherT[F, AA, D] =
    flatMap(f.andThen(EitherT.apply))

  def transform[C, D](f: Either[A, B] => Either[C, D]) given (F: Functor[F]): EitherT[F, C, D] =
    EitherT(F.map(value)(f))

  def subflatMap[AA >: A, D](f: B => Either[AA, D]) given (F: Functor[F]): EitherT[F, AA, D] =
    transform(_.flatMap(f))

  def map[D](f: B => D) given (F: Functor[F]): EitherT[F, A, D] = bimap(identity, f)

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[G[_]](f: F ~> G): EitherT[G, A, B] = EitherT[G, A, B](f(value))

  def semiflatMap[D](f: B => F[D]) given (F: Monad[F]): EitherT[F, A, D] =
    flatMap(b => EitherT.right(f(b)))

  def leftMap[C](f: A => C) given (F: Functor[F]): EitherT[F, C, B] = bimap(f, identity)

  def leftFlatMap[BB >: B, C](f: A => EitherT[F, C, BB]) given (F: Monad[F]): EitherT[F, C, BB] =
    EitherT(F.flatMap(value) {
      case Left(a)      => f(a).value
      case r @ Right(_) => F.pure(r.asInstanceOf[Either[C, BB]])
    })

  def leftSemiflatMap[D](f: A => F[D]) given (F: Monad[F]): EitherT[F, D, B] =
    EitherT(F.flatMap(value) {
      case Left(a) =>
        F.map(f(a)) { d =>
          Left(d)
        }
      case r @ Right(_) => F.pure(r.asInstanceOf[Either[D, B]])
    })

  /** Combine `leftSemiflatMap` and `semiflatMap` together.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List, String, Int] = EitherT[List, String, Int](List(Left("abc"), Right(123)))
   * scala> eitherT.biSemiflatMap(string => List(string.length), int => List(int.toFloat))
   * res0: cats.data.EitherT[List,Int,Float] = EitherT(List(Left(3), Right(123.0)))
   * }}}
   */
  def biSemiflatMap[C, D](fa: A => F[C], fb: B => F[D]) given (F: Monad[F]): EitherT[F, C, D] =
    EitherT(F.flatMap(value) {
      case Left(a) =>
        F.map(fa(a)) { c =>
          Left(c)
        }
      case Right(b) =>
        F.map(fb(b)) { d =>
          Right(d)
        }
    })

  def compare(that: EitherT[F, A, B])(implicit o: Order[F[Either[A, B]]]): Int =
    o.compare(value, that.value)

  def partialCompare(that: EitherT[F, A, B])(implicit p: PartialOrder[F[Either[A, B]]]): Double =
    p.partialCompare(value, that.value)

  def ===(that: EitherT[F, A, B])(implicit eq: Eq[F[Either[A, B]]]): Boolean =
    eq.eqv(value, that.value)

  def traverse[G[_], D](f: B => G[D])(implicit traverseF: Traverse[F],
                                      applicativeG: Applicative[G]): G[EitherT[F, A, D]] =
    applicativeG.map(traverseF.traverse(value)(axb => Traverse[[a] =>> Either[A, a]].traverse(axb)(f)))(EitherT.apply)

  def foldLeft[C](c: C)(f: (C, B) => C) given (F: Foldable[F]): C =
    F.foldLeft(value, c)((c, axb) => axb match {
      case Left(_) => c
      case Right(b) => f(c, b)
    })

  def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C]) given (F: Foldable[F]): Eval[C] =
    F.foldRight(value, lc)((axb, lc) => axb match {
      case Left(_) => lc
      case Right(b) => f(b, lc)
    })

  def merge[AA >: A](implicit ev: B <:< AA, F: Functor[F]): F[AA] = F.map(value)(_.fold(identity, ev.apply))

  /**
   * Similar to `Either#combine` but mapped over an `F` context.
   *
   * Examples:
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> val l1: EitherT[Option, String, Int] = EitherT.left(Some("error 1"))
   * scala> val l2: EitherT[Option, String, Int] = EitherT.left(Some("error 2"))
   * scala> val r3: EitherT[Option, String, Int] = EitherT.right(Some(3))
   * scala> val r4: EitherT[Option, String, Int] = EitherT.right(Some(4))
   * scala> val noneEitherT: EitherT[Option, String, Int] = EitherT.left(None)
   *
   * scala> l1 combine l2
   * res0: EitherT[Option, String, Int] = EitherT(Some(Left(error 1)))
   *
   * scala> l1 combine r3
   * res1: EitherT[Option, String, Int] = EitherT(Some(Left(error 1)))
   *
   * scala> r3 combine l1
   * res2: EitherT[Option, String, Int] = EitherT(Some(Left(error 1)))
   *
   * scala> r3 combine r4
   * res3: EitherT[Option, String, Int] = EitherT(Some(Right(7)))
   *
   * scala> l1 combine noneEitherT
   * res4: EitherT[Option, String, Int] = EitherT(None)
   *
   * scala> noneEitherT combine l1
   * res5: EitherT[Option, String, Int] = EitherT(None)
   *
   * scala> r3 combine noneEitherT
   * res6: EitherT[Option, String, Int] = EitherT(None)
   *
   * scala> noneEitherT combine r4
   * res7: EitherT[Option, String, Int] = EitherT(None)
   * }}}
   */
  def combine(that: EitherT[F, A, B]) given (F: Apply[F], B: Semigroup[B]): EitherT[F, A, B] =
    EitherT(F.map2(this.value, that.value) {
      case (l @ Left(_), _) => l
      case (_, l @ Left(_)) => l
      case (Right(a), Right(b)) => Right(B.combine(a, b))
    })

  def toValidated given (F: Functor[F]): F[Validated[A, B]] =
    F.map(value) {
      case Left(a) => Validated.invalid(a)
      case Right(b) => Validated.valid(b)
    }

  def toValidatedNel given (F: Functor[F]): F[ValidatedNel[A, B]] =
    F.map(value) {
      case Left(a) => Validated.invalidNel(a)
      case Right(b) => Validated.valid(b)
    }

  def toValidatedNec given (F: Functor[F]): F[ValidatedNec[A, B]] =
    F.map(value) {
      case Left(a) => Validated.invalidNec(a)
      case Right(b) => Validated.valid(b)
    }

  /** Run this value as a `[[Validated]]` against the function and convert it back to an `[[EitherT]]`.
   *
   * The [[Applicative]] instance for `EitherT` "fails fast" - it is often useful to "momentarily" have
   * it accumulate errors instead, which is what the `[[Validated]]` data type gives us.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> type Error = String
   * scala> val v1: Validated[NonEmptyList[Error], Int] = Validated.invalidNel("error 1")
   * scala> val v2: Validated[NonEmptyList[Error], Int] = Validated.invalidNel("error 2")
   * scala> val eithert: EitherT[Option, Error, Int] = EitherT.leftT[Option, Int]("error 3")
   * scala> eithert.withValidated { v3 => (v1, v2, v3.toValidatedNel).mapN { case (i, j, k) => i + j + k } }
   * res0: EitherT[Option, NonEmptyList[Error], Int] = EitherT(Some(Left(NonEmptyList(error 1, error 2, error 3))))
   * }}}
   */
  def withValidated[C, D](f: Validated[A, B] => Validated[C, D]) given (F: Functor[F]): EitherT[F, C, D] =
    EitherT(F.map(value) {
      case Left(a) => f(Validated.invalid(a)).toEither
      case Right(b) => f(Validated.valid(b)).toEither
    })

  def show(implicit show: Show[F[Either[A, B]]]): String = show.show(value)

  /**
   * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, Either[A, *], B]`.
   *
   * An example where `toNested` can be used, is to get the `Apply.ap` function with the
   * behavior from the composed `Apply` instances from `F` and `Either[A, *]`, which is
   * inconsistent with the behavior of the `ap` from `Monad` of `EitherT`.
   *
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> val ff: EitherT[List, String, Int => String] =
   *      |   EitherT(List(Either.right(_.toString), Either.left("error")))
   * scala> val fa: EitherT[List, String, Int] =
   *      |   EitherT(List(Either.right(1), Either.right(2)))
   * scala> ff.ap(fa)
   * res0: EitherT[List,String,String] = EitherT(List(Right(1), Right(2), Left(error)))
   * scala> EitherT((ff.toNested).ap(fa.toNested).value)
   * res1: EitherT[List,String,String] = EitherT(List(Right(1), Right(2), Left(error), Left(error)))
   * }}}
   *
   */
  def toNested: Nested[F, [a] =>> Either[A, a], B] = Nested[F, [a] =>> Either[A, a], B](value)

  /**
   * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, Validated[A, *], B]`.
   *
   * Example:
   * {{{
   * scala> import cats.data.{EitherT, Validated}
   * scala> import cats.implicits._
   * scala> val f: Int => String = i => (i*2).toString
   * scala> val r1: EitherT[Option, String, Int => String] = EitherT.right(Some(f))
   * r1: cats.data.EitherT[Option,String,Int => String] = EitherT(Some(Right(<function1>)))
   * scala> val r2: EitherT[Option, String, Int] = EitherT.right(Some(10))
   * r2: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(10)))
   * scala> type ErrorOr[A] = Validated[String, A]
   * scala> (r1.toNestedValidated).ap(r2.toNestedValidated)
   * res0: cats.data.Nested[Option,ErrorOr,String] = Nested(Some(Valid(20)))
   * }}}
   */
  def toNestedValidated given (F: Functor[F]): Nested[F, [a] =>> Validated[A, a], B] =
    Nested[F, [a] =>> Validated[A, a], B](F.map(value) {
      case Left(a) => Validated.invalid(a)
      case Right(b) => Validated.valid(b)
    })

  /**
   * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, ValidatedNel[A, *], B]`.
   */
  def toNestedValidatedNel given (F: Functor[F]): Nested[F, [a] =>> ValidatedNel[A, a], B] =
    Nested[F, [a] =>> ValidatedNel[A, a], B](F.map(value) {
      case Left(a) => Validated.invalidNel(a)
      case Right(b) => Validated.valid(b)      
    })

  /**
   * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, ValidatedNec[A, *], B]`.
   */
  def toNestedValidatedNec given (F: Functor[F]): Nested[F, [a] =>> ValidatedNec[A, a], B] =
    Nested[F, [a] =>> ValidatedNec[A, a], B](F.map(value) {
      case Left(a) => Validated.invalidNec(a)
      case Right(b) => Validated.valid(b)
    })
}

object EitherT extends EitherTInstances {

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class LeftPartiallyApplied[B](private val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], A](fa: F[A]) given (F: Functor[F]): EitherT[F, A, B] = EitherT(F.map(fa)(Left(_)))
  }

  /**
   * Creates a left version of `EitherT[F, A, B]` from a `F[A]`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.left[Int](Option("err"))
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Left(err)))
   * }}}
   */
  final def left[B]: LeftPartiallyApplied[B] = new LeftPartiallyApplied[B]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class LeftTPartiallyApplied[F[_], B](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](a: A) given (F: Applicative[F]): EitherT[F, A, B] = EitherT(F.pure(Left(a)))
  }

  /**
   * Creates a left version of `EitherT[F, A, B]` from a `A`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.leftT[Option, Int]("err")
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Left(err)))
   * }}}
   */
  final def leftT[F[_], B]: LeftTPartiallyApplied[F, B] = new LeftTPartiallyApplied[F, B]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class RightPartiallyApplied[A](private val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], B](fb: F[B]) given (F: Functor[F]): EitherT[F, A, B] = EitherT(F.map(fb)(Right(_)))
  }

  /**
   * Creates a right version of `EitherT[F, A, B]` from a `F[B]`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.right[String](Option(3))
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(3)))
   * }}}
   */
  final def right[A]: RightPartiallyApplied[A] = new RightPartiallyApplied[A]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class PurePartiallyApplied[F[_], A](private val dummy: Boolean = true) extends AnyVal {
    def apply[B](b: B) given (F: Applicative[F]): EitherT[F, A, B] = right(F.pure(b))
  }

  /**
   * Creates a new `EitherT[F, A, B]` from a `B`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.pure[Option, String](3)
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(3)))
   * }}}
   */
  final def pure[F[_], A]: PurePartiallyApplied[F, A] = new PurePartiallyApplied[F, A]

  /**
   * Alias for [[pure]]
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.rightT[Option, String](3)
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(3)))
   * }}}
   */
  final def rightT[F[_], A]: PurePartiallyApplied[F, A] = pure

  /**
   * Alias for [[right]]
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = Some(3)
   * scala> val n: Option[Int] = None
   * scala> EitherT.liftF(o)
   * res0: cats.data.EitherT[Option,Nothing,Int] = EitherT(Some(Right(3)))
   * scala> EitherT.liftF(n)
   * res1: cats.data.EitherT[Option,Nothing,Int] = EitherT(None)
   * }}}
   */
  final def liftF[F[_], A, B](fb: F[B]) given (F: Functor[F]): EitherT[F, A, B] = right(fb)

  /**
   * Same as [[liftF]], but expressed as a FunctionK for use with mapK
   * {{{
   * scala> import cats._, data._, implicits._
   * scala> val a: OptionT[Eval, Int] = 1.pure[OptionT[Eval, *]]
   * scala> val b: OptionT[EitherT[Eval, String, *], Int] = a.mapK(EitherT.liftK)
   * scala> b.value.value.value
   * res0: Either[String,Option[Int]] = Right(Some(1))
   * }}}
   */
  //final def liftK[F[_], A] given (F: Functor[F]): F ~> EitherT[F, A, _] =
  //  FunctionK[F, EitherT[F, A, _]](right(_))

  /** Transforms an `Either` into an `EitherT`, lifted into the specified `Applicative`.
   *
   * Note: The return type is a FromEitherPartiallyApplied[F], which has an apply method
   * on it, allowing you to call fromEither like this:
   * {{{
   * scala> import cats.implicits._
   * scala> val t: Either[String, Int] = Either.right(3)
   * scala> EitherT.fromEither[Option](t)
   * res0: EitherT[Option, String, Int] = EitherT(Some(Right(3)))
   * }}}
   *
   * The reason for the indirection is to emulate currying type parameters.
   */
  final def fromEither[F[_]]: FromEitherPartiallyApplied[F] = new FromEitherPartiallyApplied

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class FromEitherPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](either: Either[E, A]) given (F: Applicative[F]): EitherT[F, E, A] =
      EitherT(F.pure(either))
  }

  /** Transforms an `Option` into an `EitherT`, lifted into the specified `Applicative` and using
   *  the second argument if the `Option` is a `None`.
   * {{{
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = None
   * scala> EitherT.fromOption[List](o, "Answer not known.")
   * res0: EitherT[List, String, Int]  = EitherT(List(Left(Answer not known.)))
   * scala> EitherT.fromOption[List](Some(42), "Answer not known.")
   * res1: EitherT[List, String, Int] = EitherT(List(Right(42)))
   * }}}
   */
  final def fromOption[F[_]]: FromOptionPartiallyApplied[F] = new FromOptionPartiallyApplied

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class FromOptionPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](opt: Option[A], ifNone: => E) given (F: Applicative[F]): EitherT[F, E, A] =
      EitherT(F.pure(opt.fold(Left(ifNone))(Right(_))))
  }

  /** Transforms an `F[Option]` into an `EitherT`, using the second argument if the `Option` is a `None`.
   * {{{
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = None
   * scala> EitherT.fromOptionF(List(o), "Answer not known.")
   * res0: EitherT[List, String, Int]  = EitherT(List(Left(Answer not known.)))
   * scala> EitherT.fromOptionF(List(Option(42)), "Answer not known.")
   * res1: EitherT[List, String, Int] = EitherT(List(Right(42)))
   * }}}
   */
  final def fromOptionF[F[_], E, A](fopt: F[Option[A]], ifNone: => E) given (F: Functor[F]): EitherT[F, E, A] =
    EitherT(F.map(fopt)(_.fold(Left(ifNone))(Right(_))))

  /**  If the condition is satisfied, return the given `A` in `Right`
   *  lifted into the specified `Applicative`, otherwise, return the
   *  given `E` in `Left` lifted into the specified `Applicative`.
   *
   * {{{
   * scala> import cats.Id
   * scala> import cats.data.EitherT
   * scala> val userInput = "hello world"
   * scala> EitherT.cond[Id](
   *      |   userInput.forall(_.isDigit) && userInput.size == 10,
   *      |   userInput,
   *      |   "The input does not look like a phone number")
   * res0: EitherT[Id, String, String] = EitherT(Left(The input does not look like a phone number))
   * }}}
   */
  final def cond[F[_]]: CondPartiallyApplied[F] = new CondPartiallyApplied

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class CondPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](test: Boolean, right: => A, left: => E) given (F: Applicative[F]): EitherT[F, E, A] =
      EitherT(F.pure(Either.cond(test, right, left)))
  }
}

abstract private[data] class EitherTInstances extends EitherTInstances1 {

  implicit def catsDataOrderForEitherT[F[_], L, R] given (F: Order[F[Either[L, R]]]): Order[EitherT[F, L, R]] =
    new EitherTOrder[F, L, R] {
      val F0: Order[F[Either[L, R]]] = F
    }

  implicit def catsDataShowForEitherT[F[_], L, R](implicit sh: Show[F[Either[L, R]]]): Show[EitherT[F, L, R]] =
    Contravariant[Show].contramap(sh)(_.value)

  implicit def catsDataBifunctorForEitherT[F[_]] given (F: Functor[F]): Bifunctor[[a, b] =>> EitherT[F, a, b]] =
    new EitherTBifunctor[F] {
      val F0: Functor[F] = F
    }

  implicit def catsDataTraverseForEitherT[F[_], L] given (FF: Traverse[F]): Traverse[[a] =>> EitherT[F, L, a]] =
    new EitherTTraverse[F, L] with EitherTFunctor[F, L] {
      val F0: Traverse[F] = FF
      val F: Functor[F] = FF
    }

  implicit def catsMonoidForEitherT[F[_], L, A] given (F: Monoid[F[Either[L, A]]]): Monoid[EitherT[F, L, A]] =
    new EitherTMonoid[F, L, A] { implicit val F0 = F }

  implicit def catsDataDeferForEitherT[F[_], L] given (F: Defer[F]): Defer[[a] =>> EitherT[F, L, a]] =
    new Defer[[a] =>> EitherT[F, L, a]] {
      def defer[A](fa: => EitherT[F, L, A]): EitherT[F, L, A] =
        EitherT(F.defer(fa.value))
    }
}

abstract private[data] class EitherTInstances1 extends EitherTInstances2 {

  implicit def catsSemigroupForEitherT[F[_], L, A](
    implicit F: Semigroup[F[Either[L, A]]]
  ): Semigroup[EitherT[F, L, A]] =
    new EitherTSemigroup[F, L, A] { implicit val F0 = F }

  implicit def catsDataFoldableForEitherT[F[_], L] given (F: Foldable[F]): Foldable[[a] =>> EitherT[F, L, a]] =
    new EitherTFoldable[F, L] {
      val F0: Foldable[F] = F
    }

  implicit def catsDataPartialOrderForEitherT[F[_], L, R](
    implicit F: PartialOrder[F[Either[L, R]]]
  ): PartialOrder[EitherT[F, L, R]] =
    new EitherTPartialOrder[F, L, R] {
      val F0: PartialOrder[F[Either[L, R]]] = F
    }

  implicit def catsDataBitraverseForEitherT[F[_]] given (F: Traverse[F]): Bitraverse[[a, b] =>> EitherT[F, a, b]] =
    new EitherTBitraverse[F] with EitherTBifunctor[F] {
      val F0: Traverse[F] = F
    }

  implicit def catsDataMonadErrorForEitherT[F[_], L] given (F0: Monad[F]): MonadError[[a] =>> EitherT[F, L, a], L] =
    new EitherTMonadError[F, L] {
      implicit val F = F0
      override def ensure[A](fa: EitherT[F, L, A])(error: => L)(predicate: (A) => Boolean): EitherT[F, L, A] =
        fa.ensure(error)(predicate)

      override def ensureOr[A](fa: EitherT[F, L, A])(error: (A) => L)(predicate: (A) => Boolean): EitherT[F, L, A] =
        fa.ensureOr(error)(predicate)
    }
}

abstract private[data] class EitherTInstances2 extends EitherTInstances3 {

  /**  Monad error instance for recovering errors in F instead of
   *  the underlying Either.
   *
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.MonadError
   * scala> import cats.instances.option._
   * scala> val noInt: Option[Either[String, Int]] = None
   * scala> val et = EitherT[Option, String, Int](noInt)
   * scala> val me = MonadError[EitherT[Option, String, *], Unit]
   * scala> me.recover(et) { case () => 1 }
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(1)))
   * }}}
   */
  implicit def catsDataMonadErrorFForEitherT[F[_], E, L](
    implicit FE0: MonadError[F, E]
  ): MonadError[[a] =>> EitherT[F, L, a], E] =
    new EitherTMonadErrorF[F, E, L] { implicit val F = FE0 }

  implicit def catsDataSemigroupKForEitherT[F[_], L] given (F0: Monad[F]): SemigroupK[[a] =>> EitherT[F, L, a]] =
    new EitherTSemigroupK[F, L] { implicit val F = F0 }

  implicit def catsDataEqForEitherT[F[_], L, R] given (F: Eq[F[Either[L, R]]]): Eq[EitherT[F, L, R]] =
    new EitherTEq[F, L, R] {
      val F0: Eq[F[Either[L, R]]] = F
    }
}

abstract private[data] class EitherTInstances3 {
  implicit def catsDataFunctorForEitherT[F[_], L] given (F0: Functor[F]): Functor[[a] =>> EitherT[F, L, a]] =
    new EitherTFunctor[F, L] { implicit val F = F0 }
}

private[data] trait EitherTSemigroup[F[_], L, A] extends Semigroup[EitherT[F, L, A]] {
  implicit val F0: Semigroup[F[Either[L, A]]]
  def combine(x: EitherT[F, L, A], y: EitherT[F, L, A]): EitherT[F, L, A] =
    EitherT(F0.combine(x.value, y.value))
}

private[data] trait EitherTMonoid[F[_], L, A] extends Monoid[EitherT[F, L, A]] with EitherTSemigroup[F, L, A] {
  implicit val F0: Monoid[F[Either[L, A]]]
  def empty: EitherT[F, L, A] = EitherT(F0.empty)
}

private[data] trait EitherTSemigroupK[F[_], L] extends SemigroupK[[a] =>> EitherT[F, L, a]] {
  implicit val F: Monad[F]
  def combineK[A](x: EitherT[F, L, A], y: EitherT[F, L, A]): EitherT[F, L, A] =
    EitherT(F.flatMap(x.value) {
      case l @ Left(_)  => y.value
      case r @ Right(_) => F.pure(r)
    })
}

private[data] trait EitherTFunctor[F[_], L] extends Functor[[a] =>> EitherT[F, L, a]] {
  implicit val F: Functor[F]
  override def map[A, B](fa: EitherT[F, L, A])(f: A => B): EitherT[F, L, B] = fa.map(f)
}

private[data] trait EitherTMonad[F[_], L] extends Monad[[a] =>> EitherT[F, L, a]] with EitherTFunctor[F, L] {
  implicit val F: Monad[F]
  def pure[A](a: A): EitherT[F, L, A] = EitherT.pure(a)

  def flatMap[A, B](fa: EitherT[F, L, A])(f: A => EitherT[F, L, B]): EitherT[F, L, B] = fa.flatMap(f)
  def tailRecM[A, B](a: A)(f: A => EitherT[F, L, Either[A, B]]): EitherT[F, L, B] =
    EitherT(
      F.tailRecM(a)(
        a0 =>
          F.map(f(a0).value) {
            case Left(l)         => Right(Left(l))
            case Right(Left(a1)) => Left(a1)
            case Right(Right(b)) => Right(Right(b))
          }
      )
    )
}

private[data] trait EitherTMonadErrorF[F[_], E, L] extends MonadError[[a] =>> EitherT[F, L, a], E] with EitherTMonad[F, L] {
  implicit val F: MonadError[F, E]

  def handleErrorWith[A](fea: EitherT[F, L, A])(f: E => EitherT[F, L, A]): EitherT[F, L, A] =
    EitherT(F.handleErrorWith(fea.value)(f(_).value))

  def raiseError[A](e: E): EitherT[F, L, A] = EitherT(F.raiseError(e))
}

private[data] trait EitherTMonadError[F[_], L] extends MonadError[[a] =>> EitherT[F, L, a], L] with EitherTMonad[F, L] {
  def handleErrorWith[A](fea: EitherT[F, L, A])(f: L => EitherT[F, L, A]): EitherT[F, L, A] =
    EitherT(F.flatMap(fea.value) {
      case Left(e)      => f(e).value
      case r @ Right(_) => F.pure(r)
    })
  override def handleError[A](fea: EitherT[F, L, A])(f: L => A): EitherT[F, L, A] =
    EitherT(F.flatMap(fea.value) {
      case Left(e)      => F.pure(Right(f(e)))
      case r @ Right(_) => F.pure(r)
    })
  def raiseError[A](e: L): EitherT[F, L, A] = EitherT.left(F.pure(e))
  override def attempt[A](fla: EitherT[F, L, A]): EitherT[F, L, Either[L, A]] = EitherT.right(fla.value)
  override def recover[A](fla: EitherT[F, L, A])(pf: PartialFunction[L, A]): EitherT[F, L, A] =
    fla.recover(pf)
  override def recoverWith[A](fla: EitherT[F, L, A])(pf: PartialFunction[L, EitherT[F, L, A]]): EitherT[F, L, A] =
    fla.recoverWith(pf)
}

sealed private[data] trait EitherTFoldable[F[_], L] extends Foldable[[a] =>> EitherT[F, L, a]] {
  implicit def F0: Foldable[F]

  def foldLeft[A, B](fa: EitherT[F, L, A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def foldRight[A, B](fa: EitherT[F, L, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldRight(lb)(f)
}

sealed private[data] trait EitherTTraverse[F[_], L] extends Traverse[[a] =>> EitherT[F, L, a]] with EitherTFoldable[F, L] {
  implicit override def F0: Traverse[F]

  override def traverse[G[_]: Applicative, A, B](fa: EitherT[F, L, A])(f: A => G[B]): G[EitherT[F, L, B]] =
    fa.traverse(f)
}

sealed private[data] trait EitherTBifoldable[F[_]] extends Bifoldable[[a, b] =>> EitherT[F, a, b]] {
  implicit def F0: Foldable[F]

  def bifoldLeft[A, B, C](fab: EitherT[F, A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F0.foldLeft(fab.value, c)((acc, axb) => Bifoldable[Either].bifoldLeft(axb, acc)(f, g))

  def bifoldRight[A, B, C](fab: EitherT[F, A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C],
                                                              g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F0.foldRight(fab.value, c)((axb, acc) => Bifoldable[Either].bifoldRight(axb, acc)(f, g))
}

sealed private[data] trait EitherTBitraverse[F[_]] extends Bitraverse[[a, b] =>> EitherT[F, a, b]] with EitherTBifoldable[F] {
  implicit override def F0: Traverse[F]

  override def bitraverse[G[_], A, B, C, D](
    fab: EitherT[F, A, B]
  )(f: A => G[C], g: B => G[D]) given (G: Applicative[G]): G[EitherT[F, C, D]] =
    fab.bitraverse(f, g)
}

sealed private[data] trait EitherTBifunctor[F[_]] extends Bifunctor[[a, b] =>> EitherT[F, a, b]] {
  implicit def F0: Functor[F]

  override def bimap[A, B, C, D](fab: EitherT[F, A, B])(f: A => C, g: B => D): EitherT[F, C, D] = fab.bimap(f, g)
}

sealed private[data] trait EitherTEq[F[_], L, A] extends Eq[EitherT[F, L, A]] {
  implicit def F0: Eq[F[Either[L, A]]]

  override def eqv(x: EitherT[F, L, A], y: EitherT[F, L, A]): Boolean = x === y
}

sealed private[data] trait EitherTPartialOrder[F[_], L, A]
    extends PartialOrder[EitherT[F, L, A]]
    with EitherTEq[F, L, A] {
  implicit override def F0: PartialOrder[F[Either[L, A]]]

  override def partialCompare(x: EitherT[F, L, A], y: EitherT[F, L, A]): Double =
    x.partialCompare(y)
}

sealed private[data] trait EitherTOrder[F[_], L, A] extends Order[EitherT[F, L, A]] with EitherTPartialOrder[F, L, A] {
  implicit override def F0: Order[F[Either[L, A]]]

  override def compare(x: EitherT[F, L, A], y: EitherT[F, L, A]): Int = x.compare(y)
}
