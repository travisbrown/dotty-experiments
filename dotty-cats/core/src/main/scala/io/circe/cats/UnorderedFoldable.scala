package io.circe.cats

import io.circe.cats.kernel.{CommutativeMonoid, Order}
import scala.collection.immutable.{SortedMap, SortedSet}

/**
 * `UnorderedFoldable` is like a `Foldable` for unordered containers.
 */
trait UnorderedFoldable[F[_]] {

  def unorderedFoldMap[A, B](fa: F[A])(f: A => B) given CommutativeMonoid[B]: B

  def unorderedFold[A](fa: F[A]) given CommutativeMonoid[A]: A =
    unorderedFoldMap(fa)(identity)

  /**
   * Returns true if there are no elements. Otherwise false.
   */
  def isEmpty[A](fa: F[A]): Boolean =
    !nonEmpty(fa)

  def nonEmpty[A](fa: F[A]): Boolean =
    exists(fa)(Function.const(true))

  /**
   * Check whether at least one element satisfies the predicate.
   *
   * If there are no elements, the result is `false`.
   */
  def exists[A](fa: F[A])(p: A => Boolean): Boolean =
    (unorderedFoldMap(fa)(a => Eval.later(p(a))) given UnorderedFoldable.orEvalMonoid).value

  /**
   * Check whether all elements satisfy the predicate.
   *
   * If there are no elements, the result is `true`.
   */
  def forall[A](fa: F[A])(p: A => Boolean): Boolean =
    (unorderedFoldMap(fa)(a => Eval.later(p(a))) given UnorderedFoldable.andEvalMonoid).value

  /**
   * The size of this UnorderedFoldable.
   *
   * This is overridden in structures that have more efficient size implementations
   * (e.g. Vector, Set, Map).
   *
   * Note: will not terminate for infinite-sized collections.
   */
  def size[A](fa: F[A]): Long = unorderedFoldMap(fa)(_ => 1L) given CommutativeMonoid.instance(0, _ + _)
}

object UnorderedFoldable {
  def apply[F[_]] given (F: UnorderedFoldable[F]): UnorderedFoldable[F] = F

  given as NonEmptyTraverse[Id] = io.circe.cats.instances.IdInstance
  given as Traverse[Option] = io.circe.cats.instances.OptionInstance
  given as Traverse[List] = io.circe.cats.instances.ListInstance
  given as Traverse[Vector] = io.circe.cats.instances.VectorInstance
  given as Traverse[Stream] = io.circe.cats.instances.StreamInstance
  given as UnorderedTraverse[Set] = io.circe.cats.instances.SetInstance
  given as Foldable[SortedSet] = io.circe.cats.instances.SortedSetInstance
  given [K] as Traverse[[x] =>> SortedMap[K, x]] given Order[K] = io.circe.cats.instances.SortedMapInstance[K]

  given [A] as (Reducible[[x] =>> (A, x)] & Traverse[[x] =>> (A, x)]) = io.circe.cats.instances.Tuple2Instance[A]

  private[cats] trait Ops {
    given [F[_], A] given (F: UnorderedFoldable[F]) {
      def (fa: F[A]) size: Long = F.size(fa)
    }
  }

  private val orEvalMonoid: CommutativeMonoid[Eval[Boolean]] = new CommutativeMonoid[Eval[Boolean]] {
    val empty: Eval[Boolean] = Eval.False

    def combine(lx: Eval[Boolean], ly: Eval[Boolean]): Eval[Boolean] =
      lx.flatMap {
        case true  => Eval.True
        case false => ly
      }
  }

  private val andEvalMonoid: CommutativeMonoid[Eval[Boolean]] = new CommutativeMonoid[Eval[Boolean]] {
    val empty: Eval[Boolean] = Eval.True

    def combine(lx: Eval[Boolean], ly: Eval[Boolean]): Eval[Boolean] =
      lx.flatMap {
        case true  => ly
        case false => Eval.False
      }
  }
}
