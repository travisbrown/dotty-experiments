package io.circe.cats.instances

import io.circe.cats.{Always, Applicative, Eval, FlatMap, Foldable, MonoidK, Traverse, TraverseFilter}
import io.circe.cats.kernel.{Monoid, Order} 
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.language.implicitConversions

private[cats] class SortedMapInstance[K] given (K: Order[K]) extends Traverse[[x] =>> SortedMap[K, x]]
  with FlatMap[[x] =>> SortedMap[K, x]] 
  with TraverseFilter[[x] =>> SortedMap[K, x]] {
  lazy val ordering: Ordering[K] = Order[K].toOrdering

  def traverse[G[_], A, B](fa: SortedMap[K, A])(f: A => G[B]) given (G: Applicative[G]): G[SortedMap[K, B]] = {
    val gba: Eval[G[SortedMap[K, B]]] = Always(G.pure(SortedMap.empty(ordering)))
    Foldable
      .iterateRight(fa, gba) { (kv, lbuf) =>
        G.map2Eval(f(kv._2), lbuf)({ (b, buf) =>
          buf + (kv._1 -> b)
        })
      }
      .value
  }

  def flatMap[A, B](fa: SortedMap[K, A])(f: A => SortedMap[K, B]): SortedMap[K, B] = {
    given as Ordering[K] = ordering
    fa.flatMap { case (k, a) => f(a).get(k).map((k, _)) }
  }

  override def map[A, B](fa: SortedMap[K, A])(f: A => B): SortedMap[K, B] = {
    given as Ordering[K] = ordering
    fa.map { case (k, a) => (k, f(a)) }
  }

  override def map2Eval[A, B, Z](fa: SortedMap[K, A],
                                 fb: Eval[SortedMap[K, B]])(f: (A, B) => Z): Eval[SortedMap[K, Z]] =
    if (fa.isEmpty) Eval.now(SortedMap.empty(ordering)) // no need to evaluate fb
    else fb.map(fb => map2(fa, fb)(f))

  override def ap2[A, B, Z](f: SortedMap[K, (A, B) => Z])(fa: SortedMap[K, A],
                                                          fb: SortedMap[K, B]): SortedMap[K, Z] = {
    given as Ordering[K] = ordering
    f.flatMap {
      case (k, f) =>
        for { a <- fa.get(k); b <- fb.get(k) } yield (k, f(a, b))
    }
  }

  def foldLeft[A, B](fa: SortedMap[K, A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b) { case (x, (k, a)) => f(x, a) }

  def foldRight[A, B](fa: SortedMap[K, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable.iterateRight(fa.values, lb)(f)

  def tailRecM[A, B](a: A)(f: A => SortedMap[K, Either[A, B]]): SortedMap[K, B] = {
    val bldr = SortedMap.newBuilder[K, B](Order[K].toOrdering)

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

  override def size[A](fa: SortedMap[K, A]): Long = fa.size.toLong

  override def get[A](fa: SortedMap[K, A])(idx: Long): Option[A] =
    if (idx < 0L || Int.MaxValue < idx) None
    else {
      val n = idx.toInt
      if (n >= fa.size) None
      else Some(fa.valuesIterator.drop(n).next)
    }

  override def isEmpty[A](fa: SortedMap[K, A]): Boolean = fa.isEmpty

  override def fold[A](fa: SortedMap[K, A]) given (A: Monoid[A]): A =
    A.combineAll(fa.values)

  override def toList[A](fa: SortedMap[K, A]): List[A] = fa.values.toList

  override def collectFirst[A, B](fa: SortedMap[K, A])(pf: PartialFunction[A, B]): Option[B] =
    fa.collectFirst(new PartialFunction[(K, A), B] {
      override def isDefinedAt(x: (K, A)) = pf.isDefinedAt(x._2)
      override def apply(v1: (K, A)) = pf(v1._2)
    })

  override def collectFirstSome[A, B](fa: SortedMap[K, A])(f: A => Option[B]): Option[B] =
    collectFirst(fa)(Function.unlift(f))

  def traverse: Traverse[[a] =>> SortedMap[K, a]] = this

  override def traverseFilter[G[_], A, B](
    fa: SortedMap[K, A]
  )(f: A => G[Option[B]]) given (G: Applicative[G]): G[SortedMap[K, B]] = {
    given as Ordering[K] = ordering

    val gba: Eval[G[SortedMap[K, B]]] = Always(G.pure(SortedMap.empty))
    Foldable
      .iterateRight(fa, gba) { (kv, lbuf) =>
        G.map2Eval(f(kv._2), lbuf)({ (ob, buf) =>
          ob.fold(buf)(b => buf + (kv._1 -> b))
        })
      }
      .value
  }

  override def mapFilter[A, B](fa: SortedMap[K, A])(f: (A) => Option[B]): SortedMap[K, B] = {
    given as Ordering[K] = ordering
    fa.collect(Function.unlift(t => f(t._2).map(t._1 -> _)))
  }

  override def collect[A, B](fa: SortedMap[K, A])(f: PartialFunction[A, B]): SortedMap[K, B] = {
    given as Ordering[K] = ordering
    fa.collect(Function.unlift(t => f.lift(t._2).map(t._1 -> _)))
  }

  override def flattenOption[A](fa: SortedMap[K, Option[A]]): SortedMap[K, A] = {
    given as Ordering[K] = ordering
    fa.collect(Function.unlift(t => t._2.map(t._1 -> _)))
  }

  override def filter[A](fa: SortedMap[K, A])(f: (A) => Boolean): SortedMap[K, A] =
    fa.filter { case (_, v) => f(v) }

  override def filterA[G[_], A](
    fa: SortedMap[K, A]
  )(f: (A) => G[Boolean]) given (G: Applicative[G]): G[SortedMap[K, A]] =
    traverseFilter(fa)(a => G.map(f(a))(if (_) Some(a) else None))
}
