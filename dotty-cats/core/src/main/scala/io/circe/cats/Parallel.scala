package io.circe.cats

import io.circe.cats.arrow.FunctionK

/**
 * Some types that form a FlatMap, are also capable of forming an Apply that supports parallel composition.
 * The NonEmptyParallel type class allows us to represent this relationship.
 */
trait NonEmptyParallel[M[_], F[_]] extends Serializable {

  /**
   * The Apply instance for F[_]
   */
  def apply: Apply[F]

  /**
   * The FlatMap instance for M[_]
   */
  def flatMap: FlatMap[M]

  /**
   * Natural Transformation from the parallel Apply F[_] to the sequential FlatMap M[_].
   */
  def sequential: F ~> M

  /**
   * Natural Transformation from the sequential FlatMap M[_] to the parallel Apply F[_].
   */
  def parallel: M ~> F

  /**
   * Like [[Apply.productR]], but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parProductR[A, B](ma: M[A])(mb: M[B]): M[B] =
    flatMap.map(Parallel.parProduct(ma, mb) given this)((_, b) => b)

  /**
   * Like [[Apply.productL]], but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parProductL[A, B](ma: M[A])(mb: M[B]): M[A] =
    flatMap.map(Parallel.parProduct(ma, mb) given this)((a, _) => a)
}

/**
 * Some types that form a Monad, are also capable of forming an Applicative that supports parallel composition.
 * The Parallel type class allows us to represent this relationship.
 */
trait Parallel[M[_], F[_]] extends NonEmptyParallel[M, F] {

  /**
   * The applicative instance for F[_]
   */
  def applicative: Applicative[F]

  /**
   * The monad instance for M[_]
   */
  def monad: Monad[M]

  override def apply: Apply[F] = applicative

  override def flatMap: FlatMap[M] = monad

  /**
   * Provides an `ApplicativeError[F, E]` instance for any F, that has a `Parallel[M, F]`
   * and a `MonadError[M, E]` instance.
   * I.e. if you have a type M[_], that supports parallel composition through type F[_],
   * then you can get `ApplicativeError[F, E]` from `MonadError[M, E]`.
   */
  def applicativeError[E] given (M: MonadError[M, E]): ApplicativeError[F, E] = new ApplicativeError[F, E] {

    def raiseError[A](e: E): F[A] =
      parallel(M.raiseError(e))

    def handleErrorWith[A](fa: F[A])(f: (E) => F[A]): F[A] = {
      val ma = M.handleErrorWith(sequential(fa))(f.andThen(sequential.apply))
      parallel(ma)
    }

    def pure[A](x: A): F[A] = applicative.pure(x)

    def ap[A, B](ff: F[(A) => B])(fa: F[A]): F[B] = applicative.ap(ff)(fa)

    override def map[A, B](fa: F[A])(f: (A) => B): F[B] = applicative.map(fa)(f)

    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = applicative.product(fa, fb)

    override def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] = applicative.map2(fa, fb)(f)

    override def map2Eval[A, B, Z](fa: F[A], fb: Eval[F[B]])(f: (A, B) => Z): Eval[F[Z]] =
      applicative.map2Eval(fa, fb)(f)

    override def unlessA[A](cond: Boolean)(f: => F[A]): F[Unit] = applicative.unlessA(cond)(f)

    override def whenA[A](cond: Boolean)(f: => F[A]): F[Unit] = applicative.whenA(cond)(f)
  }
}

object NonEmptyParallel {
  def apply[M[_], F[_]] given (M: NonEmptyParallel[M, F]): NonEmptyParallel[M, F] = M
}

object Parallel {

  def apply[M[_], F[_]](M: Parallel[M, F]): Parallel[M, F] = M

  /**
   * Like `Traverse[A].sequence`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parSequence[T[_], M[_], F[_], A](tma: T[M[A]]) given (M: Parallel[M, F], T: Traverse[T]): M[T[A]] = {
    val fta: F[T[A]] = T.traverse(tma)(M.parallel.apply) given M.applicative
    M.sequential(fta)
  }

  /**
   * Like `Traverse[A].traverse`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parTraverse[T[_], M[_], F[_], A, B](ta: T[A])(f: A => M[B]) given (M: Parallel[M, F], T: Traverse[T]): M[T[B]] = {
    val gtb: F[T[B]] = T.traverse(ta)(f.andThen(M.parallel.apply)) given M.applicative
    M.sequential(gtb)
  }

  /**
   * Like `Traverse[A].flatTraverse`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parFlatTraverse[T[_], M[_], F[_], A, B](
    ta: T[A]
  )(f: A => M[T[B]]) given (M: Parallel[M, F], T: Traverse[T], FM: FlatMap[T]): M[T[B]] = {
    val gtb: F[T[B]] = T.flatTraverse(ta)(f.andThen(M.parallel.apply)) given (M.applicative, FM)
    M.sequential(gtb)
  }

  /**
   * Like `Traverse[A].flatSequence`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parFlatSequence[T[_], M[_], F[_], A](tma: T[M[T[A]]]) given (M: Parallel[M, F], T: Traverse[T], FM: FlatMap[T]): M[T[A]] = {
    val fta: F[T[A]] = Traverse[T].flatTraverse(tma)(M.parallel.apply) given (M.applicative, FM)
    M.sequential(fta)
  }

  /**
   * Like `Foldable[A].sequence_`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parSequence_[T[_], M[_], F[_], A](tma: T[M[A]]) given (M: Parallel[M, F], T: Foldable[T]): M[Unit] = {
    val fu: F[Unit] = T.traverse_(tma)(M.parallel.apply) given M.applicative
    M.sequential(fu)
  }

  /**
   * Like `Foldable[A].traverse_`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parTraverse_[T[_], M[_], F[_], A, B](ta: T[A])(f: A => M[B]) given (M: Parallel[M, F], T: Foldable[T]): M[Unit] = {
    val gtb: F[Unit] = T.traverse_(ta)(f.andThen(M.parallel.apply)) given M.applicative
    M.sequential(gtb)
  }

  /**
   * Like `NonEmptyTraverse[A].nonEmptySequence`, but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parNonEmptySequence[T[_]: NonEmptyTraverse, M[_], F[_], A](
    tma: T[M[A]]
  ) given (M: NonEmptyParallel[M, F], T: NonEmptyTraverse[T]): M[T[A]] = {
    val fta: F[T[A]] = T.nonEmptyTraverse(tma)(M.parallel.apply) given M.apply
    M.sequential(fta)
  }

  /**
   * Like `NonEmptyTraverse[A].nonEmptyTraverse`, but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parNonEmptyTraverse[T[_], M[_], F[_], A, B](
    ta: T[A]
  )(f: A => M[B]) given (M: NonEmptyParallel[M, F], T: NonEmptyTraverse[T]): M[T[B]] = {
    val gtb: F[T[B]] = T.nonEmptyTraverse(ta)(f.andThen(M.parallel.apply)) given M.apply
    M.sequential(gtb)
  }

  /**
   * Like `NonEmptyTraverse[A].nonEmptyFlatTraverse`, but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parNonEmptyFlatTraverse[T[_], M[_], F[_], A, B](
    ta: T[A]
  )(f: A => M[T[B]]) given (M: NonEmptyParallel[M, F], T: NonEmptyTraverse[T], FM: FlatMap[T]): M[T[B]] = {
    val gtb: F[T[B]] = T.nonEmptyFlatTraverse(ta)(f.andThen(M.parallel.apply)) given (M.apply, FM)
    M.sequential(gtb)
  }

  /**
   * Like `NonEmptyTraverse[A].nonEmptyFlatSequence`, but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parNonEmptyFlatSequence[T[_], M[_], F[_], A](
    tma: T[M[T[A]]]
  ) given (M: NonEmptyParallel[M, F], T: NonEmptyTraverse[T], FM: FlatMap[T]): M[T[A]] = {
    val fta: F[T[A]] = T.nonEmptyFlatTraverse(tma)(M.parallel.apply) given (M.apply, FM)
    M.sequential(fta)
  }

  /**
   * Like `Reducible[A].nonEmptySequence_`, but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parNonEmptySequence_[T[_], M[_], F[_], A](
    tma: T[M[A]]
  ) given (M: NonEmptyParallel[M, F], T: Reducible[T]): M[Unit] = {
    val fu: F[Unit] = T.nonEmptyTraverse_(tma)(M.parallel.apply) given M.apply
    M.sequential(fu)
  }

  /**
   * Like `Reducible[A].nonEmptyTraverse_`, but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parNonEmptyTraverse_[T[_], M[_], F[_], A, B](
    ta: T[A]
  )(f: A => M[B]) given (M: Parallel[M, F], T: Reducible[T]): M[Unit] = {
    val gtb: F[Unit] = T.nonEmptyTraverse_(ta)(f.andThen(M.parallel.apply)) given M.apply
    M.sequential(gtb)
  }

  /**
   * Like `Bitraverse[A].bitraverse`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parBitraverse[T[_, _], M[_], F[_], A, B, C, D](
    tab: T[A, B]
  )(f: A => M[C], g: B => M[D]) given (M: Parallel[M, F], T: Bitraverse[T]): M[T[C, D]] = {
    val ftcd: F[T[C, D]] =
      T.bitraverse(tab)(f.andThen(M.parallel.apply), g.andThen(M.parallel.apply)) given M.applicative
    M.sequential(ftcd)
  }

  /**
   * Like `Bitraverse[A].bisequence`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parBisequence[T[_, _], M[_], F[_], A, B](
    tmamb: T[M[A], M[B]]
  ) given (M: Parallel[M, F], T: Bitraverse[T]): M[T[A, B]] = {
    val ftab: F[T[A, B]] = T.bitraverse(tmamb)(M.parallel.apply, M.parallel.apply) given M.applicative
    M.sequential(ftab)
  }

  /**
   * Like `Bitraverse[A].leftTraverse`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parLeftTraverse[T[_, _], M[_], F[_], A, B, C](
    tab: T[A, B]
  )(f: A => M[C]) given (M: Parallel[M, F], T: Bitraverse[T]): M[T[C, B]] = {
    val ftcb: F[T[C, B]] =
      T.bitraverse(tab)(f.andThen(M.parallel.apply), M.applicative.pure) given M.applicative
    M.sequential(ftcb)
  }

  /**
   * Like `Bitraverse[A].leftSequence`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parLeftSequence[T[_, _], M[_], F[_], A, B](
    tmab: T[M[A], B]
  ) given (M: Parallel[M, F], T: Bitraverse[T]): M[T[A, B]] = {
    val ftab: F[T[A, B]] = T.bitraverse(tmab)(M.parallel.apply, M.applicative.pure) given M.applicative
    M.sequential(ftab)
  }

  /**
   * Like `Applicative[F].ap`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parAp[M[_], F[_], A, B](mf: M[A => B])(ma: M[A]) given (M: NonEmptyParallel[M, F]): M[B] =
    M.sequential(M.apply.ap(M.parallel(mf))(M.parallel(ma)))

  /**
   * Like `Applicative[F].product`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parProduct[M[_], F[_], A, B](ma: M[A], mb: M[B]) given (M: NonEmptyParallel[M, F]): M[(A, B)] =
    M.sequential(M.apply.product(M.parallel(ma), M.parallel(mb)))

  /**
   * Like `Applicative[F].ap2`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parAp2[M[_], F[_], A, B, Z](ff: M[(A, B) => Z])(ma: M[A], mb: M[B]) given (M: NonEmptyParallel[M, F]): M[Z] =
    M.sequential(
      M.apply.ap2(M.parallel(ff))(M.parallel(ma), M.parallel(mb))
    )

  /**
   * Provides an `ApplicativeError[F, E]` instance for any F, that has a `Parallel[M, F]`
   * and a `MonadError[M, E]` instance.
   * I.e. if you have a type M[_], that supports parallel composition through type F[_],
   * then you can get `ApplicativeError[F, E]` from `MonadError[M, E]`.
   */
  def applicativeError[M[_], F[_], E] given Parallel[M, F], MonadError[M, E]: ApplicativeError[F, E] =
    the[Parallel[M, F]].applicativeError

  /**
   * A Parallel instance for any type `M[_]` that supports parallel composition through itself.
   * Can also be used for giving `Parallel` instances to types that do not support parallel composition,
   * but are required to have an instance of `Parallel` defined,
   * in which case parallel composition will actually be sequential.
   */
  def identity[M[_]] given (M: Monad[M]): Parallel[M, M] = new Parallel[M, M] {
    val monad: Monad[M] = M
    val applicative: Applicative[M] = M
    val sequential: M ~> M = FunctionK.id
    val parallel: M ~> M = FunctionK.id
  }
}
