/*
 * Copyright (c) 2018 Luka Jacobowitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.circe.cats.data

import io.circe.cats.{Applicative, Apply, Bimonad, Eval, Foldable, Monad, NonEmptyTraverse, SemigroupK, Show}
import io.circe.cats.kernel.{Eq, Monoid, Order, PartialOrder, Semigroup}
import scala.annotation.tailrec
import scala.collection.immutable._
import scala.collection.mutable.ListBuffer

opaque type NonEmptyChain[+A] = Chain[A]

object NonEmptyChain extends NonEmptyChainInstances1 {
  def fromChain[A](as: Chain[A]): Option[NonEmptyChain[A]] =
    if (as.nonEmpty) Option(as) else None

  def fromChainUnsafe[A](chain: Chain[A]): NonEmptyChain[A] =
    if (chain.nonEmpty) chain
    else throw new IllegalArgumentException("Cannot create NonEmptyChain from empty chain")

  def fromNonEmptyList[A](as: NonEmptyList[A]): NonEmptyChain[A] =
    Chain.fromSeq(as.toList)

  def fromNonEmptyVector[A](as: NonEmptyVector[A]): NonEmptyChain[A] =
    Chain.fromSeq(as.toVector)

  def fromSeq[A](as: Seq[A]): Option[NonEmptyChain[A]] =
    if (as.nonEmpty) Option(Chain.fromSeq(as)) else None

  def fromChainPrepend[A](a: A, ca: Chain[A]): NonEmptyChain[A] =
    a +: ca

  def fromChainAppend[A](ca: Chain[A], a: A): NonEmptyChain[A] =
    ca :+ a

  def apply[A](a: A, as: A*): NonEmptyChain[A] =
    Chain.concat(Chain.one(a), Chain.fromSeq(as))

  def one[A](a: A): NonEmptyChain[A] = Chain.one(a)

  given NonEmptyChainOps[A] {

    /**
     * Converts this chain to a `Chain`
     */
    def (value: NonEmptyChain[A]) toChain: Chain[A] = value

    /**
     * Returns a new NonEmptyChain consisting of `a` followed by this. O(1) runtime.
     */
    def (value: NonEmptyChain[A]) prepend[A2 >: A](a: A2): NonEmptyChain[A2] =
      value.prepend(a)

    /**
     * Alias for [[prepend]].
     */
    def (value: NonEmptyChain[A]) +:[A2 >: A](a: A2): NonEmptyChain[A2] =
      value.prepend(a)

    /**
     * Returns a new Chain consisting of this followed by `a`. O(1) runtime.
     */
    def (value: NonEmptyChain[A]) append[A2 >: A](a: A2): NonEmptyChain[A2] =
      value.append(a)

    /**
     * Alias for [[append]].
     */
    def (value: NonEmptyChain[A]) :+[A2 >: A](a: A2): NonEmptyChain[A2] =
      value.append(a)

    /**
     * Concatenates this with `c` in O(1) runtime.
     * {{{
     * scala> import cats.data.NonEmptyChain
     * scala> val nec = NonEmptyChain(1, 2, 4, 5)
     * scala> nec ++ NonEmptyChain(7, 8)
     * res0: cats.data.NonEmptyChain[Int] = Chain(1, 2, 4, 5, 7, 8)
     * }}}
     */
    def (value: NonEmptyChain[A]) concat[A2 >: A](c: NonEmptyChain[A2]): NonEmptyChain[A2] =
      value ++ c

    /**
     * Alias for concat
     */
    def (value: NonEmptyChain[A]) ++[A2 >: A](c: NonEmptyChain[A2]): NonEmptyChain[A2] =
      value ++ c

    /**
     * Appends the given chain in O(1) runtime.
     */
    def (value: NonEmptyChain[A]) appendChain[A2 >: A](c: Chain[A2]): NonEmptyChain[A2] =
      if (c.isEmpty) value
      else value ++ c

    /**
     * Alias for `appendChain`
     * {{{
     * scala> import cats.data.NonEmptyChain
     * scala> val nec = NonEmptyChain(1, 2, 4, 5)
     * scala> nec :++ Chain(3, 6, 9)
     * res0: cats.data.NonEmptyChain[Int] = Chain(1, 2, 4, 5, 3, 6, 9)
     * }}}
     */
    def (value: NonEmptyChain[A]) :++[A2 >: A](c: Chain[A2]): NonEmptyChain[A2] =
      value.appendChain(c)

    /**
     * Prepends the given chain in O(1) runtime.
     */
    def (value: NonEmptyChain[A]) prependChain[A2 >: A](c: Chain[A2]): NonEmptyChain[A2] =
      if (c.isEmpty) value
      else c ++ value

    /**
     * Alias for `prependChain`
     * {{{
     * scala> import cats.data.NonEmptyChain
     * scala> val nec = NonEmptyChain(4, 5, 6)
     * scala> Chain(1, 2, 3) ++: nec
     * res0: cats.data.NonEmptyChain[Int] = Chain(1, 2, 3, 4, 5, 6)
     * }}}
     */
    def (value: NonEmptyChain[A]) ++:[A2 >: A](c: Chain[A2]): NonEmptyChain[A2] =
      prependChain(value)(c)

    /**
     * Yields to Some(a, Chain[A]) with `a` removed where `f` holds for the first time,
     * otherwise yields None, if `a` was not found
     * Traverses only until `a` is found.
     */
    def (value: NonEmptyChain[A]) deleteFirst(f: A => Boolean): Option[(A, Chain[A])] =
      value.deleteFirst(f)

    /**
     * Converts this chain to a `NonEmptyList`.
     * {{{
     * scala> import cats.data.NonEmptyChain
     * scala> val nec = NonEmptyChain(1, 2, 3, 4, 5)
     * scala> nec.toNonEmptyList
     * res0: cats.data.NonEmptyList[Int] = NonEmptyList(1, 2, 3, 4, 5)
     * }}}
     */
    def (value: NonEmptyChain[A]) toNonEmptyList: NonEmptyList[A] = NonEmptyList.fromListUnsafe(value.toList)

    /**
     * Converts this chain to a `NonEmptyVector`.
     * {{{
     * scala> import cats.data.NonEmptyChain
     * scala> val nec = NonEmptyChain(1, 2, 3, 4, 5)
     * scala> nec.toNonEmptyVector
     * res0: cats.data.NonEmptyVector[Int] = NonEmptyVector(1, 2, 3, 4, 5)
     * }}}
     */
    def (value: NonEmptyChain[A]) toNonEmptyVector: NonEmptyVector[A] = NonEmptyVector.fromVectorUnsafe(value.toVector)

    /**
     * Returns the head and tail of this NonEmptyChain. Amortized O(1).
     */
    def (value: NonEmptyChain[A]) uncons: (A, Chain[A]) = value.uncons.get

    /**
     * Returns the first element of this chain.
     */
    def (value: NonEmptyChain[A]) head: A = value.uncons.get._1

    /**
     * Returns all but the first element of this chain.
     */
    def (value: NonEmptyChain[A]) tail: Chain[A] = value.uncons.get._2

    /**
     * Tests if some element is contained in this chain.
     * {{{
     * scala> import cats.data.NonEmptyChain
     * scala> import cats.implicits._
     * scala> val nec = NonEmptyChain(4, 5, 6)
     * scala> nec.contains(5)
     * res0: Boolean = true
     * }}}
     */
    def (value: NonEmptyChain[A]) contains(a: A) given Eq[A]: Boolean = value.contains(a)

    /**
     * Tests whether a predicate holds for all elements of this chain.
     */
    def (value: NonEmptyChain[A]) forall(p: A => Boolean): Boolean = value.forall(p)

    /**
     * Tests whether a predicate holds for at least one element of this chain.
     */
    def (value: NonEmptyChain[A]) exists(f: A => Boolean): Boolean = value.exists(f)

    /**
     * Returns the first value that matches the given predicate.
     */
    def (value: NonEmptyChain[A]) find(f: A => Boolean): Option[A] = value.find(f)

    /**
     * Returns a new `Chain` containing all elements where the result of `pf` is defined.
     * {{{
     * scala> import cats.data.NonEmptyChain
     * scala> import cats.implicits._
     * scala> val nec = NonEmptyChain(4, 5, 6).map(n => if (n % 2 == 0) Some(n) else None)
     * scala> nec.collect { case Some(n) => n }
     * res0: cats.data.Chain[Int] = Chain(4, 6)
     * }}}
     */
    def (value: NonEmptyChain[A]) collect[B](pf: PartialFunction[A, B]): Chain[B] = value.collect(pf)

    /**
     * Finds the first element of this `NonEmptyChain` for which the given partial
     * function is defined, and applies the partial function to it.
     */
    def (value: NonEmptyChain[A]) collectFirst[B](pf: PartialFunction[A, B]): Option[B] = value.collectFirst(pf)

    /**
     * Like `collectFirst` from `scala.collection.Traversable` but takes `A => Option[B]`
     * instead of `PartialFunction`s.
     */
    def (value: NonEmptyChain[A]) collectFirstSome[B](f: A => Option[B]): Option[B] = value.collectFirstSome(f)

    /**
     * Filters all elements of this chain that do not satisfy the given predicate.
     */
    def (value: NonEmptyChain[A]) filter(p: A => Boolean): Chain[A] = value.filter(p)

    /**
     * Filters all elements of this chain that satisfy the given predicate.
     */
    def (value: NonEmptyChain[A]) filterNot(p: A => Boolean): Chain[A] = filter(value)(t => !p(t))

    /**
     * Left-associative fold using f.
     */
    def (value: NonEmptyChain[A]) foldLeft[B](b: B)(f: (B, A) => B): B =
      value.foldLeft(b)(f)

    /**
     * Right-associative fold using f.
     */
    def (value: NonEmptyChain[A]) foldRight[B](z: B)(f: (A, B) => B): B =
      value.foldRight(z)(f)

    /**
     * Left-associative reduce using f.
     * {{{
     * scala> import cats.data.NonEmptyChain
     * scala> val nec = NonEmptyChain(4, 5, 6)
     * scala> nec.reduceLeft(_ + _)
     * res0: Int = 15
     * }}}
     */
    def (value: NonEmptyChain[A]) reduceLeft(f: (A, A) => A): A = {
      val iter = value.iterator
      var result = iter.next
      while (iter.hasNext) { result = f(result, iter.next) }
      result
    }

    /**
     * Apply `f` to the "initial element" of this chain and lazily combine it
     * with every other value using the given function `g`.
     * {{{
     * scala> import cats.data.NonEmptyChain
     * scala> val nec = NonEmptyChain(4, 5, 6)
     * scala> nec.reduceLeftTo(_.toString)((acc, cur) => acc + cur.toString)
     * res0: String = 456
     * }}}
     */
    def (value: NonEmptyChain[A]) reduceLeftTo[B](f: A => B)(g: (B, A) => B): B = {
      val iter = value.iterator
      var result = f(iter.next)
      while (iter.hasNext) { result = g(result, iter.next) }
      result
    }

    /**
     * Right-associative reduce using f.
     * {{{
     * scala> import cats.data.NonEmptyChain
     * scala> val nec = NonEmptyChain(4, 5, 6)
     * scala> nec.reduceRight(_ + _)
     * res0: Int = 15
     * }}}
     */
    def (value: NonEmptyChain[A]) reduceRight(f: (A, A) => A): A = {
      val iter = value.reverseIterator
      var result = iter.next
      while (iter.hasNext) { result = f(result, iter.next) }
      result
    }

    /**
     * Apply `f` to the "initial element" of this chain and lazily combine it
     * with every other value using the given function `g`.
     * {{{
     * scala> import cats.data.NonEmptyChain
     * scala> val nec = NonEmptyChain(4, 5, 6)
     * scala> nec.reduceRightTo(_.toString)((cur, acc) => acc + cur.toString)
     * res0: String = 654
     * }}}
     */
    def (value: NonEmptyChain[A]) reduceRightTo[B](f: A => B)(g: (A, B) => B): B = {
      val iter = value.reverseIterator
      var result = f(iter.next)
      while (iter.hasNext) { result = g(iter.next, result) }
      result
    }

    /**
     * Reduce using the Semigroup of A
     */
    def (value: NonEmptyChain[A]) reduce[AA >: A] given (S: Semigroup[AA]): AA =
      S.combineAllOption(value.iterator).get

    /**
     * Applies the supplied function to each element and returns a new NonEmptyChain from the concatenated results
     */
    def (value: NonEmptyChain[A]) flatMap[B](f: A => NonEmptyChain[B]): NonEmptyChain[B] =
      value.flatMap(f)

    /**
     * Returns the number of elements in this chain.
     */
    def (value: NonEmptyChain[A]) length: Long = value.size

    /**
     * Zips this `NonEmptyChain` with another `NonEmptyChain` and applies a function for each pair of elements.
     *
     * {{{
     * scala> import cats.data.NonEmptyChain
     * scala> val as = NonEmptyChain(1, 2, 3)
     * scala> val bs = NonEmptyChain("A", "B", "C")
     * scala> as.zipWith(bs)(_ + _)
     * res0: cats.data.NonEmptyChain[String] = Chain(1A, 2B, 3C)
     * }}}
     */
    def (value: NonEmptyChain[A]) zipWith[B, C](b: NonEmptyChain[B])(f: (A, B) => C): NonEmptyChain[C] =
      value.zipWith(b)(f)

    /**
     * Groups elements inside this `NonEmptyChain` according to the `Order`
     * of the keys produced by the given mapping function.
     */
    def (value: NonEmptyChain[A]) groupBy[B](f: A => B) given Order[B]: NonEmptyMap[B, NonEmptyChain[A]] =
      value.groupBy(f).asInstanceOf[NonEmptyMap[B, NonEmptyChain[A]]]

    def (value: NonEmptyChain[A]) iterator: Iterator[A] = value.iterator

    def (value: NonEmptyChain[A]) reverseIterator: Iterator[A] = value.reverseIterator

    /** Reverses this `NonEmptyChain` */
    def (value: NonEmptyChain[A]) reverse: NonEmptyChain[A] =
      value.reverse

    /**
     * Remove duplicates. Duplicates are checked using `Order[_]` instance.
     */
    def (value: NonEmptyChain[A]) distinct[AA >: A] given (AA: Order[AA]): NonEmptyChain[AA] = {
      implicit val ord: Ordering[AA] = AA.toOrdering

      var alreadyIn = TreeSet(value.head: AA)

      value.foldLeft(Chain(value.head: AA)) { (elementsSoFar, b) =>
        if (alreadyIn.contains(b)) {
          elementsSoFar
        } else {
          alreadyIn += b
          elementsSoFar :+ b
        }
      }
    }
  }

  given NonEmptyChainInstance as (SemigroupK[NonEmptyChain] & NonEmptyTraverse[NonEmptyChain] & Bimonad[NonEmptyChain]) =
    new SemigroupK[NonEmptyChain] with NonEmptyTraverse[NonEmptyChain] with Bimonad[NonEmptyChain] {

      def combineK[A](a: NonEmptyChain[A], b: NonEmptyChain[A]): NonEmptyChain[A] =
        a ++ b

      def pure[A](x: A): NonEmptyChain[A] = NonEmptyChain.one(x)

      def flatMap[A, B](fa: NonEmptyChain[A])(f: A => NonEmptyChain[B]): NonEmptyChain[B] =
        fa.flatMap(f)

      def tailRecM[A, B](a: A)(f: A => NonEmptyChain[Either[A, B]]): NonEmptyChain[B] =
        Chain.ChainInstance.tailRecM(a)(a => f(a))

      def extract[A](x: NonEmptyChain[A]): A = x.head

      def coflatMap[A, B](fa: NonEmptyChain[A])(f: NonEmptyChain[A] => B): NonEmptyChain[B] = {
        @tailrec def go(as: Chain[A], res: ListBuffer[B]): Chain[B] =
          as.uncons match {
            case Some((h, t)) => go(t, res += f(NonEmptyChain.fromChainPrepend(h, t)))
            case None         => Chain.fromSeq(res.result())
          }
        NonEmptyChain.fromChainPrepend(f(fa), go(fa.tail, ListBuffer.empty))
      }

      def nonEmptyTraverse[G[_], A, B](fa: NonEmptyChain[A])(f: A => G[B]) given Apply[G]: G[NonEmptyChain[B]] =
        Chain.ChainInstance
          .reduceRightToOption[A, G[Chain[B]]](fa.tail)(a => Apply[G].map(f(a))(Chain.one)) { (a, lglb) =>
            Apply[G].map2Eval(f(a), lglb)(_ +: _)
          }
          .map {
            case None        => Apply[G].map(f(fa.head))(NonEmptyChain.one)
            case Some(gtail) => Apply[G].map2(f(fa.head), gtail)((h, t) => Chain.one(h) ++ t)
          }
          .value

      override def map[A, B](fa: NonEmptyChain[A])(f: A => B): NonEmptyChain[B] =
        fa.toChain.map(f)

      override def size[A](fa: NonEmptyChain[A]): Long = fa.length

      override def reduceLeft[A](fa: NonEmptyChain[A])(f: (A, A) => A): A =
        fa.reduceLeft(f)

      override def reduce[A](fa: NonEmptyChain[A]) given (A: Semigroup[A]): A =
        fa.reduce

      def reduceLeftTo[A, B](fa: NonEmptyChain[A])(f: A => B)(g: (B, A) => B): B = fa.reduceLeftTo(f)(g)

      def reduceRightTo[A, B](fa: NonEmptyChain[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        Eval.defer(fa.reduceRightTo(a => Eval.now(f(a))) { (a, b) =>
          Eval.defer(g(a, b))
        })

      override def foldLeft[A, B](fa: NonEmptyChain[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: NonEmptyChain[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Chain.ChainInstance.foldRight(fa.toChain, lb)(f)

      override def foldMap[A, B](fa: NonEmptyChain[A])(f: A => B) given (B: Monoid[B]): B =
        B.combineAll(fa.toChain.iterator.map(f))

      override def fold[A](fa: NonEmptyChain[A]) given (A: Monoid[A]): A =
        fa.reduce

      override def find[A](fa: NonEmptyChain[A])(f: A => Boolean): Option[A] =
        fa.find(f)

      override def forall[A](fa: NonEmptyChain[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def exists[A](fa: NonEmptyChain[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def toList[A](fa: NonEmptyChain[A]): List[A] = fa.toChain.toList

      override def toNonEmptyList[A](fa: NonEmptyChain[A]): NonEmptyList[A] =
        fa.toNonEmptyList

      override def collectFirst[A, B](fa: NonEmptyChain[A])(pf: PartialFunction[A, B]): Option[B] =
        fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: NonEmptyChain[A])(f: A => Option[B]): Option[B] =
        fa.collectFirstSome(f)
    }

  given [A] as Order[NonEmptyChain[A]] given Order[A] = Chain.OrderForChain[A]

  given [A] as Show[NonEmptyChain[A]] given (A: Show[A]) =
    Show.show[NonEmptyChain[A]](nec => s"NonEmpty${Show[Chain[A]].show(nec.toChain)}")

  given [A] as Semigroup[NonEmptyChain[A]] {
    def combine(x: NonEmptyChain[A], y: NonEmptyChain[A]): NonEmptyChain[A] = x ++ y
  }
}

sealed abstract private[data] class NonEmptyChainInstances1 extends NonEmptyChainInstances2 {
  given [A] as PartialOrder[NonEmptyChain[A]] given PartialOrder[A] =
    PartialOrder.by[NonEmptyChain[A], Chain[A]](_.toChain)
}

sealed abstract private[data] class NonEmptyChainInstances2 {
  given [A] as Eq[NonEmptyChain[A]] given Eq[A] {
    def eqv(x: NonEmptyChain[A], y: NonEmptyChain[A]): Boolean = x.toChain === y.toChain
  }
}
