package io.circe.cats

import io.circe.cats.data.State
import io.circe.cats.data.StateT
import io.circe.cats.kernel.Order
import scala.collection.immutable.SortedMap

/**
 * Traverse, also known as Traversable.
 *
 * Traversal over a structure with an effect.
 *
 * Traversing with the [[cats.Id]] effect is equivalent to [[cats.Functor]]#map.
 * Traversing with the [[cats.data.Const]] effect where the first type parameter has
 * a [[cats.Monoid]] instance is equivalent to [[cats.Foldable]]#fold.
 *
 * See: [[https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence of the Iterator Pattern]]
 */
trait Traverse[F[_]] extends Functor[F] with Foldable[F] with UnorderedTraverse[F] { self =>

  /**
   * Given a function which returns a G effect, thread this effect
   * through the running of this function on all the values in F,
   * returning an F[B] in a G context.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption
   * scala> List("1", "2", "3").traverse(parseInt)
   * res0: Option[List[Int]] = Some(List(1, 2, 3))
   * scala> List("1", "two", "3").traverse(parseInt)
   * res1: Option[List[Int]] = None
   * }}}
   */
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B]) given Applicative[G]: G[F[B]]

  /**
   * A traverse followed by flattening the inner result.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption
   * scala> val x = Option(List("1", "two", "3"))
   * scala> x.flatTraverse(_.map(parseInt))
   * res0: List[Option[Int]] = List(Some(1), None, Some(3))
   * }}}
   */
  def flatTraverse[G[_], A, B](fa: F[A])(f: A => G[F[B]]) given (G: Applicative[G], F: FlatMap[F]): G[F[B]] =
    G.map(traverse(fa)(f))(F.flatten)

  /**
   * Thread all the G effects through the F structure to invert the
   * structure from F[G[A]] to G[F[A]].
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val x: List[Option[Int]] = List(Some(1), Some(2))
   * scala> val y: List[Option[Int]] = List(None, Some(2))
   * scala> x.sequence
   * res0: Option[List[Int]] = Some(List(1, 2))
   * scala> y.sequence
   * res1: Option[List[Int]] = None
   * }}}
   */
  def sequence[G[_], A](fga: F[G[A]]) given Applicative[G]: G[F[A]] =
    traverse(fga)(ga => ga)

  /**
   * Thread all the G effects through the F structure and flatten to invert the
   * structure from F[G[F[A]]] to G[F[A]].
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val x: List[Option[List[Int]]] = List(Some(List(1, 2)), Some(List(3)))
   * scala> val y: List[Option[List[Int]]] = List(None, Some(List(3)))
   * scala> x.flatSequence
   * res0: Option[List[Int]] = Some(List(1, 2, 3))
   * scala> y.flatSequence
   * res1: Option[List[Int]] = None
   * }}}
   */
  def flatSequence[G[_], A](fgfa: F[G[F[A]]]) given (G: Applicative[G], F: FlatMap[F]): G[F[A]] =
    G.map(sequence(fgfa))(F.flatten)

  def compose[G[_]] given Traverse[G]: Traverse[[x] =>> F[G[x]]] =
    new ComposedTraverse[F, G] {
      val F = self
      val G = the[Traverse[G]]
    }

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)

  /**
   * Akin to [[map]], but also provides the value's index in structure
   * F when calling the function.
   */
  def mapWithIndex[A, B](fa: F[A])(f: (A, Int) => B): F[B] =
    traverse(fa)(a => State((s: Int) => (s + 1, f(a, s)))).runA(0).value

  /**
   * Akin to [[traverse]], but also provides the value's index in
   * structure F when calling the function.
   *
   * This performs the traversal in a single pass but requires that
   * effect G is monadic. An applicative traversal can be performed in
   * two passes using [[zipWithIndex]] followed by [[traverse]].
   */
  def traverseWithIndexM[G[_], A, B](fa: F[A])(f: (A, Int) => G[B]) given (G: Monad[G]): G[F[B]] =
    traverse(fa)(a => StateT((s: Int) => G.map(f(a, s))(b => (s + 1, b)))).runA(0)

  /**
   * Traverses through the structure F, pairing the values with
   * assigned indices.
   *
   * The behavior is consistent with the Scala collection library's
   * `zipWithIndex` for collections such as `List`.
   */
  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapWithIndex(fa)((a, i) => (a, i))

  override def unorderedTraverse[G[_], A, B](sa: F[A])(f: (A) => G[B]) given CommutativeApplicative[G]: G[F[B]] =
    traverse(sa)(f)

  override def unorderedSequence[G[_], A](fga: F[G[A]]) given CommutativeApplicative[G]: G[F[A]] =
    sequence(fga)
}

object Traverse {
  def apply[F[_]] given (F: Traverse[F]): Traverse[F] = F

  given [A] as Traverse[[x] =>> Either[A, x]] = io.circe.cats.instances.EitherInstance[A]
  given [K] as Traverse[[x] =>> SortedMap[K, x]] given Order[K] = io.circe.cats.instances.SortedMapInstance[K]

  private[cats] trait Ops {
    given [F[_], A] {
      def (fa: F[A]) traverse[G[_], B](f: A => G[B]) given (F: Traverse[F], G: Applicative[G]): G[F[B]] = F.traverse(fa)(f)
      def (fa: F[A]) flatTraverse[G[_], B](f: A => G[F[B]]) given Traverse[F], Applicative[G], FlatMap[F]: G[F[B]] =
        the[Traverse[F]].flatTraverse(fa)(f)
      def (fa: F[A]) traverseWithIndexM[G[_], B](f: (A, Int) => G[B]) given (F: Traverse[F], G: Monad[G]): G[F[B]] =
        F.traverseWithIndexM(fa)(f)
      def (fa: F[A]) zipWithIndex given (F: Traverse[F]): F[(A, Int)] = F.zipWithIndex(fa)
    }
  }
}
