package io.circe.cats.instances

import io.circe.cats.{Eval, Foldable, SemigroupK, Semigroupal}
import io.circe.cats.kernel.{BoundedSemilattice, Eq, Hash, Monoid, Order}
import scala.collection.immutable.SortedSet
import scala.annotation.tailrec

private[cats] object SortedSetInstance extends Foldable[SortedSet] with SemigroupK[SortedSet] with Semigroupal[SortedSet] {
  def combineK[A](x: SortedSet[A], y: SortedSet[A]): SortedSet[A] = x | y

  def foldLeft[A, B](fa: SortedSet[A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def foldRight[A, B](fa: SortedSet[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable.iterateRight(fa, lb)(f)

  override def foldMap[A, B](fa: SortedSet[A])(f: A => B) given (B: Monoid[B]): B =
    B.combineAll(fa.iterator.map(f))

  override def get[A](fa: SortedSet[A])(idx: Long): Option[A] = {
    @tailrec
    def go(idx: Int, it: Iterator[A]): Option[A] =
      if (it.hasNext) {
        if (idx == 0) Some(it.next)
        else {
          it.next
          go(idx - 1, it)
        }
      } else None
    if (idx < Int.MaxValue && idx >= 0L) go(idx.toInt, fa.iterator) else None
  }

  override def size[A](fa: SortedSet[A]): Long = fa.size.toLong

  override def exists[A](fa: SortedSet[A])(p: A => Boolean): Boolean =
    fa.exists(p)

  override def forall[A](fa: SortedSet[A])(p: A => Boolean): Boolean =
    fa.forall(p)

  override def isEmpty[A](fa: SortedSet[A]): Boolean = fa.isEmpty

  override def fold[A](fa: SortedSet[A]) given (A: Monoid[A]): A = A.combineAll(fa)

  override def toList[A](fa: SortedSet[A]): List[A] = fa.toList

  override def reduceLeftOption[A](fa: SortedSet[A])(f: (A, A) => A): Option[A] =
    fa.reduceLeftOption(f)

  override def find[A](fa: SortedSet[A])(f: A => Boolean): Option[A] = fa.find(f)

  override def collectFirst[A, B](fa: SortedSet[A])(pf: PartialFunction[A, B]): Option[B] =
    fa.collectFirst(pf)

  override def collectFirstSome[A, B](fa: SortedSet[A])(f: A => Option[B]): Option[B] =
    fa.collectFirst(Function.unlift(f))

  override def product[A, B](fa: SortedSet[A], fb: SortedSet[B]): SortedSet[(A, B)] = {
    given as Ordering[A] = fa.ordering
    given as Ordering[B] = fb.ordering

    fa.flatMap(a => fb.map(b => a -> b))
  }
}
