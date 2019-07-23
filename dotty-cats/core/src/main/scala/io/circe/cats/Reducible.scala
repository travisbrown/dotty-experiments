package io.circe.cats

import io.circe.cats.data.{Ior, NonEmptyList}
import io.circe.cats.kernel.{Order, Semigroup}

/**
 * Data structures that can be reduced to a summary value.
 *
 * `Reducible` is like a non-empty `Foldable`. In addition to the fold
 * methods it provides reduce methods which do not require an initial
 * value.
 *
 * In addition to the methods needed by `Foldable`, `Reducible` is
 * implemented in terms of two methods:
 *
 *  - `reduceLeftTo(fa)(f)(g)` eagerly reduces with an additional mapping function
 *  - `reduceRightTo(fa)(f)(g)` lazily reduces with an additional mapping function
 */
trait Reducible[F[_]] extends Foldable[F] { self =>

  /**
   * Left-associative reduction on `F` using the function `f`.
   *
   * Implementations should override this method when possible.
   */
  def reduceLeft[A](fa: F[A])(f: (A, A) => A): A =
    reduceLeftTo(fa)(identity)(f)

  /**
   * Right-associative reduction on `F` using the function `f`.
   */
  def reduceRight[A](fa: F[A])(f: (A, Eval[A]) => Eval[A]): Eval[A] =
    reduceRightTo(fa)(identity)(f)

  /**
   * Reduce a `F[A]` value using the given `Semigroup[A]`.
   */
  def reduce[A](fa: F[A]) given (A: Semigroup[A]): A =
    reduceLeft(fa)(A.combine)

  /**
   * Reduce a `F[G[A]]` value using `SemigroupK[G]`, a universal
   * semigroup for `G[_]`.
   *
   * This method is a generalization of `reduce`.
   */
  def reduceK[G[_], A](fga: F[G[A]]) given (G: SemigroupK[G]): G[A] =
    reduce(fga) given G.algebra

  /**
   * Apply `f` to each element of `fa` and combine them using the
   * given `Semigroup[B]`.
   */
  def reduceMap[A, B](fa: F[A])(f: A => B) given (B: Semigroup[B]): B =
    reduceLeftTo(fa)(f)((b, a) => B.combine(b, f(a)))

  /**
   * Apply `f` to the "initial element" of `fa` and combine it with
   * every other value using the given function `g`.
   */
  def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B

  /**
   *  Monadic variant of [[reduceLeftTo]]
   */
  def reduceLeftM[G[_], A, B](fa: F[A])(f: A => G[B])(g: (B, A) => G[B]) given (G: FlatMap[G]): G[B] =
    reduceLeftTo(fa)(f)((gb, a) => G.flatMap(gb)(g(_, a)))

  /**
   * Monadic reducing by mapping the `A` values to `G[B]`. combining
   * the `B` values using the given `Semigroup[B]` instance.
   *
   * Similar to [[reduceLeftM]], but using a `Semigroup[B]`.
   *
   * {{{
   * scala> import cats.Reducible
   * scala> import cats.data.NonEmptyList
   * scala> import cats.implicits._
   * scala> val evenOpt: Int => Option[Int] =
   *      |   i => if (i % 2 == 0) Some(i) else None
   * scala> val allEven = NonEmptyList.of(2,4,6,8,10)
   * allEven: cats.data.NonEmptyList[Int] = NonEmptyList(2, 4, 6, 8, 10)
   * scala> val notAllEven = allEven ++ List(11)
   * notAllEven: cats.data.NonEmptyList[Int] = NonEmptyList(2, 4, 6, 8, 10, 11)
   * scala> Reducible[NonEmptyList].reduceMapM(allEven)(evenOpt)
   * res0: Option[Int] = Some(30)
   * scala> Reducible[NonEmptyList].reduceMapM(notAllEven)(evenOpt)
   * res1: Option[Int] = None
   * }}}
   */
  def reduceMapM[G[_], A, B](fa: F[A])(f: A => G[B]) given (G: FlatMap[G], B: Semigroup[B]): G[B] =
    reduceLeftM(fa)(f)((b, a) => G.map(f(a))(B.combine(b, _)))

  /**
   * Overridden from [[Foldable]] for efficiency.
   */
  override def reduceLeftToOption[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): Option[B] =
    Some(reduceLeftTo(fa)(f)(g))

  /**
   * Apply `f` to the "initial element" of `fa` and lazily combine it
   * with every other value using the given function `g`.
   */
  def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B]

  /**
   * Overridden from [[Foldable]] for efficiency.
   */
  override def reduceRightToOption[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
    reduceRightTo(fa)(f)(g).map(Some(_))

  /**
   * Traverse `F[A]` using `Apply[G]`.
   *
   * `A` values will be mapped into `G[B]` and combined using
   * `Apply#map2`.
   *
   * This method is similar to [[Foldable.traverse_]]. There are two
   * main differences:
   *
   * 1. We only need an [[Apply]] instance for `G` here, since we
   * don't need to call [[Applicative.pure]] for a starting value.
   * 2. This performs a strict left-associative traversal and thus
   * must always traverse the entire data structure. Prefer
   * [[Foldable.traverse_]] if you have an [[Applicative]] instance
   * available for `G` and want to take advantage of short-circuiting
   * the traversal.
   */
  def nonEmptyTraverse_[G[_], A, B](fa: F[A])(f: A => G[B]) given (G: Apply[G]): G[Unit] =
    G.void(reduceLeftTo(fa)(f)((x, y) => G.map2(x, f(y))((_, b) => b)))

  /**
   * Sequence `F[G[A]]` using `Apply[G]`.
   *
   * This method is similar to [[Foldable.sequence_]] but requires only
   * an [[Apply]] instance for `G` instead of [[Applicative]]. See the
   * [[nonEmptyTraverse_]] documentation for a description of the differences.
   */
  def nonEmptySequence_[G[_], A](fga: F[G[A]]) given (G: Apply[G]): G[Unit] =
    G.void(reduceLeft(fga)((x, y) => G.map2(x, y)((_, b) => b)))

  def toNonEmptyList[A](fa: F[A]): NonEmptyList[A] =
    reduceRightTo(fa)(a => NonEmptyList(a, Nil)) { (a, lnel) =>
      lnel.map { case NonEmptyList(h, t) => NonEmptyList(a, h :: t) }
    }.value

  def compose[G[_]: Reducible]: Reducible[[x] =>> F[G[x]]] =
    new ComposedReducible[F, G] {
      val F = self
      val G = Reducible[G]
    }

  def minimum[A](fa: F[A]) given (A: Order[A]): A =
    reduceLeft(fa)(A.min)

  def maximum[A](fa: F[A]) given (A: Order[A]): A =
    reduceLeft(fa)(A.max)

  /**
   * Intercalate/insert an element between the existing elements while reducing.
   *
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of("a", "b", "c")
   * scala> Reducible[NonEmptyList].nonEmptyIntercalate(nel, "-")
   * res0: String = a-b-c
   * scala> Reducible[NonEmptyList].nonEmptyIntercalate(NonEmptyList.of("a"), "-")
   * res1: String = a
   * }}}
   */
  def nonEmptyIntercalate[A](fa: F[A], a: A) given (A: Semigroup[A]): A =
    toNonEmptyList(fa) match {
      case NonEmptyList(hd, Nil) => hd
      case NonEmptyList(hd, tl) =>
        Reducible[NonEmptyList].reduce(NonEmptyList(hd, a :: intersperseList(tl, a)))
    }

  /**
   * Partition this Reducible by a separating function `A => Either[B, C]`
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of(1,2,3,4)
   * scala> Reducible[NonEmptyList].nonEmptyPartition(nel)(a => if (a % 2 == 0) Left(a.toString) else Right(a))
   * res0: cats.data.Ior[cats.data.NonEmptyList[String],cats.data.NonEmptyList[Int]] = Both(NonEmptyList(2, 4),NonEmptyList(1, 3))
   * scala> Reducible[NonEmptyList].nonEmptyPartition(nel)(a => Right(a * 4))
   * res1: cats.data.Ior[cats.data.NonEmptyList[Nothing],cats.data.NonEmptyList[Int]] = Right(NonEmptyList(4, 8, 12, 16))
   * }}}
   */
  def nonEmptyPartition[A, B, C](fa: F[A])(f: A => Either[B, C]): Ior[NonEmptyList[B], NonEmptyList[C]] = {
    def g(a: A, eval: Eval[Ior[NonEmptyList[B], NonEmptyList[C]]]): Eval[Ior[NonEmptyList[B], NonEmptyList[C]]] =
      eval.map(
        ior =>
          (f(a), ior) match {
            case (Right(c), Ior.Left(_)) => ior.putRight(NonEmptyList.one(c))
            case (Right(c), _)           => ior.map(c :: _)
            case (Left(b), Ior.Right(r)) => Ior.bothNel(b, r)
            case (Left(b), _)            => ior.leftMap(b :: _)
          }
      )

    reduceRightTo(fa)(a => (f(a) match {
      case Left(b) => Ior.left(NonEmptyList.one(b))
      case Right(c) => Ior.right(NonEmptyList.one(c))
    }))(g).value
  }

  override def isEmpty[A](fa: F[A]): Boolean = false

  override def nonEmpty[A](fa: F[A]): Boolean = true

  override def minimumOption[A](fa: F[A]) given (A: Order[A]): Option[A] =
    Some(minimum(fa))

  override def maximumOption[A](fa: F[A]) given (A: Order[A]): Option[A] =
    Some(maximum(fa))
}

object Reducible {
  def apply[F[_]] given (F: Reducible[F]): Reducible[F] = F

  given [A] as Reducible[[x] =>> (A, x)] = io.circe.cats.instances.Tuple2Instance[A]

  private[cats] trait Ops {
    given [F[_], A] given (F: Reducible[F]) {
      def (fa: F[A]) reduceLeft(f: (A, A) => A): A = F.reduceLeft(fa)(f)
      def (fa: F[A]) reduceRight(f: (A, Eval[A]) => Eval[A]): Eval[A] = F.reduceRight(fa)(f)
      def (fa: F[A]) reduce given Semigroup[A]: A = F.reduce(fa)
      def (fa: F[A]) reduceMap[B](f: A => B) given Semigroup[B]: B = F.reduceMap(fa)(f)
      def (fa: F[A]) reduceLeftTo[B](f: A => B)(g: (B, A) => B): B = F.reduceLeftTo(fa)(f)(g)
      def (fa: F[A]) reduceRightTo[B](f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = F.reduceRightTo(fa)(f)(g)
      def (fa: F[A]) nonEmptyTraverse_[G[_], B](f: A => G[B]) given Apply[G]: G[Unit] = F.nonEmptyTraverse_(fa)(f)
      def (fa: F[A]) toNonEmptyList: NonEmptyList[A] = F.toNonEmptyList(fa)
      def (fa: F[A]) minimum given Order[A]: A = F.minimum(fa)
      def (fa: F[A]) maximum given Order[A]: A = F.maximum(fa)
    }

    given [F[_], G[_], A] given (F: Reducible[F]) {
      def (fga: F[G[A]]) nonEmptySequence_ given (G: Apply[G]): G[Unit] = F.nonEmptySequence_(fga)
    }
  }
}
