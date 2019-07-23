package io.circe.cats.instances

import io.circe.cats.{Always, CommutativeApplicative, Eval, FlatMap, Foldable, Functor, FunctorFilter, MonoidK, UnorderedTraverse}
import io.circe.cats.kernel.{CommutativeMonoid, Monoid}
import scala.annotation.tailrec
import scala.language.implicitConversions

private[cats] class MapInstance[K] extends UnorderedTraverse[[x] =>> Map[K, x]]
  with FlatMap[[x] =>> Map[K, x]]
  with FunctorFilter[[x] =>> Map[K, x]]
  with MonoidK[[x] =>> Map[K, x]] {
  def unorderedTraverse[G[_], A, B](
    fa: Map[K, A]
  )(f: A => G[B]) given (G: CommutativeApplicative[G]): G[Map[K, B]] = {
    val gba: Eval[G[Map[K, B]]] = Always(G.pure(Map.empty))
    val gbb = Foldable
      .iterateRight(fa, gba) { (kv, lbuf) =>
        G.map2Eval(f(kv._2), lbuf)({ (b, buf) =>
          buf + (kv._1 -> b)
        })
      }
      .value
    G.map(gbb)(_.toMap)
  }

  override def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] =
    fa.map { case (k, a) => (k, f(a)) }

  override def map2[A, B, Z](fa: Map[K, A], fb: Map[K, B])(f: (A, B) => Z): Map[K, Z] =
    if (fb.isEmpty) Map.empty // do O(1) work if fb is empty
    else fa.flatMap { case (k, a) => fb.get(k).map(b => (k, f(a, b))) }

  override def map2Eval[A, B, Z](fa: Map[K, A], fb: Eval[Map[K, B]])(f: (A, B) => Z): Eval[Map[K, Z]] =
    if (fa.isEmpty) Eval.now(Map.empty) // no need to evaluate fb
    else fb.map(fb => map2(fa, fb)(f))

  override def ap[A, B](ff: Map[K, A => B])(fa: Map[K, A]): Map[K, B] =
    fa.flatMap { case (k, a) => ff.get(k).map(f => (k, f(a))) }

  override def ap2[A, B, Z](f: Map[K, (A, B) => Z])(fa: Map[K, A], fb: Map[K, B]): Map[K, Z] =
    f.flatMap {
      case (k, f) =>
        for { a <- fa.get(k); b <- fb.get(k) } yield (k, f(a, b))
    }

  def flatMap[A, B](fa: Map[K, A])(f: (A) => Map[K, B]): Map[K, B] =
    fa.flatMap { case (k, a) => f(a).get(k).map((k, _)) }

  def unorderedFoldMap[A, B: CommutativeMonoid](fa: Map[K, A])(f: (A) => B) =
    fa.foldLeft(Monoid[B].empty) { case (b, (k, a)) => Monoid[B].combine(b, f(a)) }

  def tailRecM[A, B](a: A)(f: A => Map[K, Either[A, B]]): Map[K, B] = {
    val bldr = Map.newBuilder[K, B]

    @tailrec def descend(k: K, either: Either[A, B]): Unit =
      either match {
        case Left(a) =>
          f(a).get(k) match {
            case Some(x) => descend(k, x)
            case None    => ()
          }
        case Right(b) =>
          bldr += ((k, b))
          ()
      }

    f(a).foreach { case (k, a) => descend(k, a) }
    bldr.result
  }

  override def isEmpty[A](fa: Map[K, A]): Boolean = fa.isEmpty

  override def unorderedFold[A](fa: Map[K, A]) given (A: CommutativeMonoid[A]): A =
    A.combineAll(fa.values)

  override def forall[A](fa: Map[K, A])(p: A => Boolean): Boolean = fa.forall(pair => p(pair._2))

  override def exists[A](fa: Map[K, A])(p: A => Boolean): Boolean = fa.exists(pair => p(pair._2))

  def functor: Functor[[x] =>> Map[K, x]] = this

  def mapFilter[A, B](fa: Map[K, A])(f: A => Option[B]) =
    fa.collect(Function.unlift(t => f(t._2).map(t._1 -> _)))

  override def collect[A, B](fa: Map[K, A])(f: PartialFunction[A, B]) =
    fa.collect(Function.unlift(t => f.lift(t._2).map(t._1 -> _)))

  override def flattenOption[A](fa: Map[K, Option[A]]) =
    fa.collect(Function.unlift(t => t._2.map(t._1 -> _)))

  override def filter[A](fa: Map[K, A])(f: A => Boolean) =
    fa.filter { case (_, v) => f(v) }

  override def empty[A]: Map[K, A] = Map.empty

  override def combineK[A](x: Map[K, A], y: Map[K, A]): Map[K, A] = x ++ y
}
