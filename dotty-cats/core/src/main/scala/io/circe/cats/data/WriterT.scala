package io.circe.cats.data

import io.circe.cats.{Alternative, Applicative, ApplicativeError, Apply, Bifunctor, CoflatMap, Comonad, CommutativeMonad, Contravariant, ContravariantMonoidal, Defer, Eval, FlatMap, Foldable, Functor, Id, Invariant, Monad, MonadError, MonoidK, Parallel, SemigroupK, Show, Traverse, ~>}
import io.circe.cats.arrow.FunctionK
import io.circe.cats.kernel.{CommutativeMonoid, Eq, Monoid, Semigroup}
import given io.circe.cats.syntax.semigroup._

final case class WriterT[F[_], L, V](run: F[(L, V)]) {
  def tell(l: L) given Functor[F], Semigroup[L]: WriterT[F, L, V] =
    mapWritten(_ |+| l)

  def written given (F: Functor[F]): F[L] =
    F.map(run)(_._1)

  def value given (F: Functor[F]): F[V] =
    F.map(run)(_._2)

  def listen given (F: Functor[F]): WriterT[F, L, (V, L)] =
    WriterT(F.map(run) {
      case (l, v) => (l, (v, l))
    })

  def ap[Z](f: WriterT[F, L, V => Z]) given (F: Apply[F], L: Semigroup[L]): WriterT[F, L, Z] =
    WriterT(F.map2(f.run, run) {
      case ((l1, fvz), (l2, v)) => (L.combine(l1, l2), fvz(v))
    })

  def map[Z](fn: V => Z) given (F: Functor[F]): WriterT[F, L, Z] =
    WriterT {
      F.map(run) { z =>
        (z._1, fn(z._2))
      }
    }

  def imap[Z](f: V => Z)(g: Z => V) given (F: Invariant[F]): WriterT[F, L, Z] =
    WriterT {
      F.imap(run)(z => (z._1, f(z._2)))(z => (z._1, g(z._2)))
    }

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[G[_]](f: F ~> G): WriterT[G, L, V] =
    WriterT[G, L, V](f(run))

  def contramap[Z](fn: Z => V) given (F: Contravariant[F]): WriterT[F, L, Z] =
    WriterT {
      F.contramap(run) { z =>
        (z._1, fn(z._2))
      }
    }

  def flatMap[U](f: V => WriterT[F, L, U]) given (F: FlatMap[F], L: Semigroup[L]): WriterT[F, L, U] =
    WriterT {
      F.flatMap(run) { lv =>
        F.map(f(lv._2).run) { lv2 =>
          (L.combine(lv._1, lv2._1), lv2._2)
        }
      }
    }

  def mapBoth[M, U](f: (L, V) => (M, U)) given (F: Functor[F]): WriterT[F, M, U] =
    WriterT { F.map(run)(f.tupled) }

  def bimap[M, U](f: L => M, g: V => U) given Functor[F]: WriterT[F, M, U] =
    mapBoth((l, v) => (f(l), g(v)))

  def mapWritten[M](f: L => M) given Functor[F]: WriterT[F, M, V] =
    mapBoth((l, v) => (f(l), v))

  def swap given Functor[F]: WriterT[F, V, L] =
    mapBoth((l, v) => (v, l))

  def reset given (F: Functor[F], L: Monoid[L]): WriterT[F, L, V] =
    mapWritten(_ => L.empty)

  def show given (F: Show[F[(L, V)]]): String = F.show(run)

  def foldLeft[C](c: C)(f: (C, V) => C) given (F: Foldable[F]): C =
    F.foldLeft(run, c)((z, lv) => f(z, lv._2))

  def foldRight[C](lc: Eval[C])(f: (V, Eval[C]) => Eval[C]) given (F: Foldable[F]): Eval[C] =
    F.foldRight(run, lc)((lv, z) => f(lv._2, z))

  def traverse[G[_], V1](f: V => G[V1]) given (F: Traverse[F], G: Applicative[G]): G[WriterT[F, L, V1]] =
    G.map(
      F.traverse(run)(lv => G.tupleLeft(f(lv._2), lv._1))
    )(WriterT.apply)
}

object WriterT extends WriterTInstances with WriterTFunctions with WriterTFunctions0 {

  def liftF[F[_], L, V](fv: F[V]) given (F: Applicative[F], L: Monoid[L]): WriterT[F, L, V] =
    WriterT(F.map(fv)(v => (L.empty, v)))

  /**
   * Same as [[liftF]], but expressed as a FunctionK for use with mapK
   * {{{
   * scala> import cats._, data._, implicits._
   * scala> val a: OptionT[Eval, Int] = 1.pure[OptionT[Eval, *]]
   * scala> val b: OptionT[WriterT[Eval, String, *], Int] = a.mapK(WriterT.liftK)
   * scala> b.value.run.value
   * res0: (String, Option[Int]) = ("",Some(1))
   * }}}
   */
  def liftK[F[_], L] given (F: Applicative[F], L: Monoid[L]): FunctionK[F, [x] =>> WriterT[F, L, x]] =
    new FunctionK[F, [x] =>> WriterT[F, L, x]] {
      def apply[X](fx: F[X]) = WriterT.liftF(fx)
    }
}

sealed abstract private[data] class WriterTInstances extends WriterTInstances0 {

  given [F[_], L] as CommutativeMonad[[x] =>> WriterT[F, L, x]] given (F: CommutativeMonad[F], L: CommutativeMonoid[L]) =
    new WriterTMonad[F, L] with CommutativeMonad[[x] =>> WriterT[F, L, x]] {
      implicit val F0: Monad[F] = F
      implicit val L0: Monoid[L] = L
    }

  given [L] as Traverse[[x] =>> WriterT[Id, L, x]] = TraverseForWriterT[Id, L]

  given [F[_], L] as Defer[[x] =>> WriterT[F, L, x]] given (F: Defer[F]) {
    def defer[A](fa: => WriterT[F, L, A]): WriterT[F, L, A] =
      WriterT(F.defer(fa.run))
  }
}

sealed abstract private[data] class WriterTInstances0 extends WriterTInstances1 {
  given TraverseForWriterT[F[_], L] as Traverse[[x] =>> WriterT[F, L, x]] given (F: Traverse[F]) =
    new WriterTTraverse[F, L] {
      val F0: Traverse[F] = F
    }

  given [L] as Foldable[[x] =>> WriterT[Id, L, x]] = FoldableForWriterT[Id, L]
}

sealed abstract private[data] class WriterTInstances1 extends WriterTInstances2 {
  given [F[_], L, E] as MonadError[[x] =>> WriterT[F, L, x], E] given (F: MonadError[F, E], L: Monoid[L])  =
    new WriterTMonadError[F, L, E] {
      implicit val F0: MonadError[F, E] = F
      implicit val L0: Monoid[L] = L
    }

  given [M[_], F[_], L] as Parallel[[x] =>> WriterT[M, L, x], [x] =>> WriterT[F, L, x]] given (P: Parallel[M, F], L: Monoid[L]) {
    given as Applicative[F] = P.applicative
    given as Monad[M] = P.monad

    def applicative: Applicative[[x] =>> WriterT[F, L, x]] = the[Applicative[[x] =>> WriterT[F, L, x]]]
    def monad: Monad[[x] =>> WriterT[M, L, x]] = the[Monad[[x] =>> WriterT[M, L, x]]]

    def sequential: FunctionK[[x] =>> WriterT[F, L, x], [x] =>> WriterT[M, L, x]] =
      new FunctionK[[x] =>> WriterT[F, L, x], [x] =>> WriterT[M, L, x]] {
        def apply[X](wfl: WriterT[F, L, X]) = WriterT(P.sequential(wfl.run))
      }

    def parallel: FunctionK[[x] =>> WriterT[M, L, x], [x] =>> WriterT[F, L, x]] =
      new FunctionK[[x] =>> WriterT[M, L, x], [x] =>> WriterT[F, L, x]] {
        def apply[X](wml: WriterT[M, L, X]) = WriterT(P.parallel(wml.run))
      }
  }

  given [F[_]] as Bifunctor[[x, y] =>> WriterT[F, x, y]] given Functor[F] {
    def bimap[A, B, C, D](fab: WriterT[F, A, B])(f: A => C, g: B => D): WriterT[F, C, D] =
      fab.bimap(f, g)
  }

  given [F[_], L, V] as Show[WriterT[F, L, V]] given Show[F[(L, V)]] {
    def show(f: WriterT[F, L, V]): String = f.show
  }

  given FoldableForWriterT[F[_], L] as Foldable[[x] =>> WriterT[F, L, x]] given (F: Foldable[F]) =
    new WriterTFoldable[F, L] {
      val F0: Foldable[F] = F
    }
}

sealed abstract private[data] class WriterTInstances2 extends WriterTInstances3 {
  given [L] as Monad[[x] =>> WriterT[Id, L, x]] given Monoid[L] = MonadForWriterT[Id, L]

  given [F[_], L, V] as Eq[WriterT[F, L, V]] given (F: Eq[F[(L, V)]]) =
    Eq.by[WriterT[F, L, V], F[(L, V)]](_.run)

  //implicit def catsDataSemigroupForWriterTId[L: Semigroup, V: Semigroup]: Semigroup[WriterT[Id, L, V]] =
  //  catsDataSemigroupForWriterT[Id, L, V]

  given [L] as Comonad[[x] =>> WriterT[Id, L, x]] =
    catsDataComonadForWriterT[Id, L]
}

sealed abstract private[data] class WriterTInstances3 extends WriterTInstances4 {
  given MonadForWriterT[F[_], L] as Monad[[x] =>> WriterT[F, L, x]] given (F: Monad[F], L: Monoid[L]) =
    new WriterTMonad[F, L] {
      implicit val F0: Monad[F] = F
      implicit val L0: Monoid[L] = L
    }

  implicit def catsDataMonoidForWriterT[F[_], L, V](implicit W: Monoid[F[(L, V)]]): Monoid[WriterT[F, L, V]] =
    new WriterTMonoid[F, L, V] {
      implicit val F0: Monoid[F[(L, V)]] = W
    }

  implicit def catsDataCoflatMapForWriterTId[L]: CoflatMap[[x] =>> WriterT[Id, L, x]] =
    catsDataCoflatMapForWriterT[Id, L]
}

sealed abstract private[data] class WriterTInstances4 extends WriterTInstances5 {
  implicit def catsDataFlatMapForWriterTId[L: Semigroup]: FlatMap[[x] =>> WriterT[Id, L, x]] =
    catsDataFlatMapForWriterT2[Id, L]
}

sealed abstract private[data] class WriterTInstances5 extends WriterTInstances6 {
  implicit def catsDataFlatMapForWriterT1[F[_], L] given (F: FlatMap[F], L: Monoid[L]): FlatMap[[x] =>> WriterT[F, L, x]] =
    new WriterTFlatMap1[F, L] {
      implicit val F0: FlatMap[F] = F
      implicit val L0: Monoid[L] = L
    }

  implicit def catsDataSemigroupForWriterT[F[_], L, V](implicit W: Semigroup[F[(L, V)]]): Semigroup[WriterT[F, L, V]] =
    new WriterTSemigroup[F, L, V] {
      implicit val F0: Semigroup[F[(L, V)]] = W
    }
}

sealed abstract private[data] class WriterTInstances6 extends WriterTInstances7 {
  implicit def catsDataApplicativeErrorForWriterT[F[_], L, E] given (F: ApplicativeError[F, E],
                                                              L: Monoid[L]): ApplicativeError[[x] =>> WriterT[F, L, x], E] =
    new WriterTApplicativeError[F, L, E] {
      implicit val F0: ApplicativeError[F, E] = F
      implicit val L0: Monoid[L] = L
    }
}

sealed abstract private[data] class WriterTInstances7 extends WriterTInstances8 {
  implicit def catsDataAlternativeForWriterT[F[_], L] given (F: Alternative[F],
                                                      L: Monoid[L]): Alternative[[x] =>> WriterT[F, L, x]] =
    new WriterTAlternative[F, L] {
      implicit val F0: Alternative[F] = F
      implicit val L0: Monoid[L] = L
    }

  implicit def catsDataContravariantMonoidalForWriterT[F[_], L](
    implicit F: ContravariantMonoidal[F]
  ): ContravariantMonoidal[[x] =>> WriterT[F, L, x]] =
    new WriterTContravariantMonoidal[F, L] {
      implicit val F0: ContravariantMonoidal[F] = F
    }
}

sealed abstract private[data] class WriterTInstances8 extends WriterTInstances9 {
  implicit def catsDataMonoidKForWriterT[F[_], L] given (F: MonoidK[F]): MonoidK[[x] =>> WriterT[F, L, x]] =
    new WriterTMonoidK[F, L] {
      implicit val F0: MonoidK[F] = F
    }

  implicit def catsDataFlatMapForWriterT2[F[_], L] given (F: Monad[F], L: Semigroup[L]): FlatMap[[x] =>> WriterT[F, L, x]] =
    new WriterTFlatMap2[F, L] {
      implicit val F0: Monad[F] = F
      implicit val L0: Semigroup[L] = L
    }

  implicit def catsDataContravariantForWriterT[F[_], L] given (F: Contravariant[F]): Contravariant[[x] =>> WriterT[F, L, x]] =
    new WriterTContravariant[F, L] {
      implicit val F0: Contravariant[F] = F
    }
}

sealed abstract private[data] class WriterTInstances9 extends WriterTInstances10 {
  implicit def catsDataSemigroupKForWriterT[F[_], L] given (F: SemigroupK[F]): SemigroupK[[x] =>> WriterT[F, L, x]] =
    new WriterTSemigroupK[F, L] {
      implicit val F0: SemigroupK[F] = F
    }

  implicit def catsDataApplicativeForWriterT[F[_], L] given (F: Applicative[F],
                                                      L: Monoid[L]): Applicative[[x] =>> WriterT[F, L, x]] =
    new WriterTApplicative[F, L] {
      implicit val F0: Applicative[F] = F
      implicit val L0: Monoid[L] = L
    }

  implicit def catsDataInvariantForWriterT[F[_], L](implicit F0: Invariant[F]): Invariant[[x] =>> WriterT[F, L, x]] =
    new WriterTInvariant[F, L] { implicit val F = F0 }
}

sealed abstract private[data] class WriterTInstances10 extends WriterTInstances11 {
  implicit def catsDataApplyForWriterT[F[_], L] given (F: Apply[F], L: Semigroup[L]): Apply[[x] =>> WriterT[F, L, x]] =
    new WriterTApply[F, L] {
      implicit val F0: Apply[F] = F
      implicit val L0: Semigroup[L] = L
    }
}

sealed abstract private[data] class WriterTInstances11 extends WriterTInstances12 {
  implicit def catsDataComonadForWriterT[F[_], L] given (F: Comonad[F]): Comonad[[x] =>> WriterT[F, L, x]] =
    new WriterTComonad[F, L] {
      implicit val F0: Comonad[F] = F
    }
}

sealed abstract private[data] class WriterTInstances12 {
  implicit def catsDataCoflatMapForWriterT[F[_], L] given (F: Functor[F]): CoflatMap[[x] =>> WriterT[F, L, x]] =
    new WriterTCoflatMap[F, L] {
      implicit val F0: Functor[F] = F
    }
}

sealed private[data] trait WriterTFunctor[F[_], L] extends Functor[[x] =>> WriterT[F, L, x]] {
  implicit def F0: Functor[F]

  override def map[A, B](fa: WriterT[F, L, A])(f: A => B): WriterT[F, L, B] =
    fa.map(f)
}

sealed private[data] trait WriterTContravariant[F[_], L] extends Contravariant[[x] =>> WriterT[F, L, x]] {
  implicit def F0: Contravariant[F]

  override def contramap[A, B](fa: WriterT[F, L, A])(f: B => A): WriterT[F, L, B] =
    fa.contramap(f)
}

sealed private[data] trait WriterTInvariant[F[_], L] extends Invariant[[x] =>> WriterT[F, L, x]] {
  implicit def F: Invariant[F]

  override def imap[A, B](fa: WriterT[F, L, A])(f: A => B)(g: B => A): WriterT[F, L, B] =
    fa.imap(f)(g)
}

sealed private[data] trait WriterTApply[F[_], L] extends WriterTFunctor[F, L] with Apply[[x] =>> WriterT[F, L, x]] {
  implicit override def F0: Apply[F]
  implicit def L0: Semigroup[L]

  def ap[A, B](f: WriterT[F, L, A => B])(fa: WriterT[F, L, A]): WriterT[F, L, B] =
    fa.ap(f)

  override def map2Eval[A, B, Z](fa: WriterT[F, L, A],
                                 fb: Eval[WriterT[F, L, B]])(f: (A, B) => Z): Eval[WriterT[F, L, Z]] =
    F0.map2Eval(fa.run, fb.map(_.run)) { case ((la, a), (lb, b)) => (L0.combine(la, lb), f(a, b)) }
      .map(WriterT(_)) // F0 may have a lazy map2Eval

  override def product[A, B](fa: WriterT[F, L, A], fb: WriterT[F, L, B]): WriterT[F, L, (A, B)] =
    WriterT(F0.map(F0.product(fa.run, fb.run)) { case ((l1, a), (l2, b)) => (L0.combine(l1, l2), (a, b)) })
}

sealed private[data] trait WriterTFlatMap1[F[_], L] extends WriterTApply[F, L] with FlatMap[[x] =>> WriterT[F, L, x]] {
  implicit override def F0: FlatMap[F]
  implicit def L0: Monoid[L]

  override def ap[A, B](f: WriterT[F, L, A => B])(fa: WriterT[F, L, A]): WriterT[F, L, B] =
    super[WriterTApply].ap(f)(fa)

  def flatMap[A, B](fa: WriterT[F, L, A])(f: A => WriterT[F, L, B]): WriterT[F, L, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(fn: A => WriterT[F, L, Either[A, B]]): WriterT[F, L, B] = {

    def step(la: (L, A)): F[Either[(L, A), (L, B)]] = {
      val flv: F[(L, Either[A, B])] = fn(la._2).run
      F0.map(flv) {
        case (l, Left(a)) =>
          val combineL = L0.combine(la._1, l)
          Left((combineL, a))
        case (l, Right(b)) =>
          val combineL = L0.combine(la._1, l)
          Right((combineL, b))
      }
    }

    WriterT(F0.tailRecM((L0.empty, a))(step))
  }
}

sealed private[data] trait WriterTFlatMap2[F[_], L] extends WriterTApply[F, L] with FlatMap[[x] =>> WriterT[F, L, x]] {
  implicit override def F0: Monad[F]
  implicit def L0: Semigroup[L]

  def flatMap[A, B](fa: WriterT[F, L, A])(f: A => WriterT[F, L, B]): WriterT[F, L, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(fn: A => WriterT[F, L, Either[A, B]]): WriterT[F, L, B] = {

    def step(la: (L, A)): F[Either[(L, A), (L, B)]] = {
      val flv: F[(L, Either[A, B])] = fn(la._2).run
      F0.map(flv) {
        case (l, Left(a)) =>
          val combineL = L0.combine(la._1, l)
          Left((combineL, a))
        case (l, Right(b)) =>
          val combineL = L0.combine(la._1, l)
          Right((combineL, b))
      }
    }

    val init = fn(a).run
    val res: F[(L, B)] = F0.flatMap(init) {
      case (l, Right(b)) => F0.pure((l, b))
      case (l, Left(a))  => F0.tailRecM((l, a))(step)
    }
    WriterT(res)
  }
}

sealed private[data] trait WriterTApplicative[F[_], L] extends WriterTApply[F, L] with Applicative[[x] =>> WriterT[F, L, x]] {
  implicit override def F0: Applicative[F]
  implicit override def L0: Monoid[L]

  def pure[A](a: A): WriterT[F, L, A] =
    WriterT.value[F, L, A](a)
}

sealed private[data] trait WriterTMonad[F[_], L]
    extends WriterTApplicative[F, L]
    with WriterTFlatMap1[F, L]
    with Monad[[x] =>> WriterT[F, L, x]] {
  implicit override def F0: Monad[F]
  implicit override def L0: Monoid[L]
}

sealed private[data] trait WriterTApplicativeError[F[_], L, E]
    extends ApplicativeError[[x] =>> WriterT[F, L, x], E]
    with WriterTApplicative[F, L] {
  implicit override def F0: ApplicativeError[F, E]

  def raiseError[A](e: E): WriterT[F, L, A] = WriterT(F0.raiseError[(L, A)](e))

  def handleErrorWith[A](fa: WriterT[F, L, A])(f: E => WriterT[F, L, A]): WriterT[F, L, A] =
    WriterT(F0.handleErrorWith(fa.run)(e => f(e).run))
}

sealed private[data] trait WriterTMonadError[F[_], L, E]
    extends MonadError[[x] =>> WriterT[F, L, x], E]
    with WriterTMonad[F, L]
    with WriterTApplicativeError[F, L, E] {
  implicit override def F0: MonadError[F, E]
}

sealed private[data] trait WriterTSemigroupK[F[_], L] extends SemigroupK[[x] =>> WriterT[F, L, x]] {
  implicit def F0: SemigroupK[F]

  def combineK[A](x: WriterT[F, L, A], y: WriterT[F, L, A]): WriterT[F, L, A] =
    WriterT(F0.combineK(x.run, y.run))
}

sealed private[data] trait WriterTMonoidK[F[_], L] extends MonoidK[[x] =>> WriterT[F, L, x]] with WriterTSemigroupK[F, L] {
  implicit override def F0: MonoidK[F]

  def empty[A]: WriterT[F, L, A] = WriterT(F0.empty)
}

sealed private[data] trait WriterTAlternative[F[_], L]
    extends Alternative[[x] =>> WriterT[F, L, x]]
    with WriterTMonoidK[F, L]
    with WriterTApplicative[F, L] {
  implicit override def F0: Alternative[F]
}

sealed private[data] trait WriterTContravariantMonoidal[F[_], L] extends ContravariantMonoidal[[x] =>> WriterT[F, L, x]] {
  implicit def F0: ContravariantMonoidal[F]

  override def unit: WriterT[F, L, Unit] = WriterT(F0.trivial[(L, Unit)])

  override def contramap[A, B](fa: WriterT[F, L, A])(f: B => A): WriterT[F, L, B] =
    fa.contramap(f)

  override def product[A, B](fa: WriterT[F, L, A], fb: WriterT[F, L, B]): WriterT[F, L, (A, B)] =
    WriterT(
      F0.contramap(F0.product(fa.run, fb.run))(
        (t: (L, (A, B))) =>
          t match {
            case (l, (a, b)) => ((l, a), (l, b))
          }
      )
    )
}

sealed private[data] trait WriterTSemigroup[F[_], L, A] extends Semigroup[WriterT[F, L, A]] {
  implicit def F0: Semigroup[F[(L, A)]]

  def combine(x: WriterT[F, L, A], y: WriterT[F, L, A]): WriterT[F, L, A] =
    WriterT(F0.combine(x.run, y.run))
}

sealed private[data] trait WriterTMonoid[F[_], L, A] extends Monoid[WriterT[F, L, A]] with WriterTSemigroup[F, L, A] {
  implicit override def F0: Monoid[F[(L, A)]]

  def empty: WriterT[F, L, A] = WriterT(F0.empty)
}

sealed private[data] trait WriterTCoflatMap[F[_], L] extends CoflatMap[[x] =>> WriterT[F, L, x]] with WriterTFunctor[F, L] {

  def coflatMap[A, B](fa: WriterT[F, L, A])(f: WriterT[F, L, A] => B): WriterT[F, L, B] = fa.map(_ => f(fa))
}

sealed private[data] trait WriterTFoldable[F[_], L] extends Foldable[[x] =>> WriterT[F, L, x]] {

  implicit def F0: Foldable[F]

  def foldLeft[A, B](fa: WriterT[F, L, A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
  def foldRight[A, B](fa: WriterT[F, L, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldRight(lb)(f)
}

sealed private[data] trait WriterTTraverse[F[_], L]
    extends Traverse[[x] =>> WriterT[F, L, x]]
    with WriterTFoldable[F, L]
    with WriterTFunctor[F, L] {

  implicit override def F0: Traverse[F]

  def traverse[G[_]: Applicative, A, B](fa: WriterT[F, L, A])(f: A => G[B]): G[WriterT[F, L, B]] = fa.traverse(f)
}

sealed private[data] trait WriterTComonad[F[_], L] extends Comonad[[x] =>> WriterT[F, L, x]] with WriterTCoflatMap[F, L] {

  implicit override def F0: Comonad[F]

  def extract[A](fa: WriterT[F, L, A]): A = F0.extract(F0.map(fa.run)(_._2))
}

// new trait for binary compatibility
private[data] trait WriterTFunctions0 {
  def listen[F[_], L, V](writerTFLV: WriterT[F, L, V]) given (F: Functor[F]): WriterT[F, L, (V, L)] =
    writerTFLV.listen
}

private[data] trait WriterTFunctions {
  def putT[F[_], L, V](vf: F[V])(l: L) given (F: Functor[F]): WriterT[F, L, V] =
    WriterT(F.map(vf)(v => (l, v)))

  def put[F[_], L, V](v: V)(l: L) given (F: Applicative[F]): WriterT[F, L, V] =
    WriterT.putT[F, L, V](F.pure(v))(l)

  def tell[F[_], L](l: L) given Applicative[F]: WriterT[F, L, Unit] =
    WriterT.put[F, L, Unit](())(l)

  def value[F[_], L, V](v: V) given (F: Applicative[F], L: Monoid[L]): WriterT[F, L, V] =
    WriterT.put[F, L, V](v)(L.empty)

  def valueT[F[_], L, V](vf: F[V]) given (F: Functor[F], L: Monoid[L]): WriterT[F, L, V] =
    WriterT.putT[F, L, V](vf)(L.empty)
}
