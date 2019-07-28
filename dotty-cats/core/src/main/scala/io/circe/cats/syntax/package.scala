package io.circe.cats.syntax

import io.circe.cats.{Applicative, Apply, Bifoldable, Bifunctor, Bitraverse, CoflatMap, Comonad, Contravariant, ContravariantSemigroupal, Distributive, FlatMap, Foldable, Functor, FunctorFilter, Invariant, Monad, NonEmptyTraverse, Reducible, Semigroupal, Show, Traverse, TraverseFilter, UnorderedFoldable, UnorderedTraverse}
import io.circe.cats.arrow.{Arrow, ArrowChoice, Choice, Compose, Profunctor, Strong}
import io.circe.cats.data.{NonEmptyChain, NonEmptyList, Writer}
import io.circe.cats.kernel.{Comparison, Eq, Hash, Monoid, Order, PartialOrder, Semigroup}
import scala.annotation.alpha
import scala.collection.immutable.{SortedMap, SortedSet}

object applicative extends Applicative.Ops
object apply extends Apply.Ops
object arrow extends Arrow.Ops
object arrowChoice extends ArrowChoice.Ops
object bifoldable extends Bifoldable.Ops
object bifunctor extends Bifunctor.Ops
object bitraverse extends Bitraverse.Ops
object choice extends Choice.Ops
object coflatMap extends CoflatMap.Ops
object comonad extends Comonad.Ops
object compose extends Compose.Ops
object contravariant extends Contravariant.Ops
object contravariantSemigroupal extends ContravariantSemigroupal.Ops
object distributive extends Distributive.Ops
object either extends EitherOps
object eq extends EqOps
object flatMap extends FlatMap.Ops
object functor extends Functor.Ops
object functorFilter extends FunctorFilter.Ops
object foldable extends Foldable.Ops
object hash extends HashOps
object invariant extends Invariant.Ops
object list extends ListOps
object monad extends Monad.Ops
object monoid extends MonoidOps
object nonEmptyTraverse extends NonEmptyTraverse.Ops
object option extends OptionOps
object order extends OrderOps
object partialOrder extends PartialOrderOps
object profunctor extends Profunctor.Ops
object reducible extends Reducible.Ops
object show extends Show.Ops
object semigroup extends SemigroupOps
object semigroupal extends Semigroupal.Ops
object strong extends Strong.Ops
object traverse extends Traverse.Ops
object traverseFilter extends TraverseFilter.Ops
object unorderedFoldable extends UnorderedFoldable.Ops
object unorderedTraverse extends UnorderedTraverse.Ops
object writer extends WriterOps
object all extends Applicative.Ops with Apply.Ops with Arrow.Ops with ArrowChoice.Ops
  with Bifoldable.Ops with Bifunctor.Ops with Bitraverse.Ops with Choice.Ops with CoflatMap.Ops with Comonad.Ops
  with Compose.Ops with Contravariant.Ops with ContravariantSemigroupal.Ops with Distributive.Ops with EitherOps with EqOps
  with FlatMap.Ops with Functor.Ops with FunctorFilter.Ops with Foldable.Ops with HashOps with Invariant.Ops with ListOps with Monad.Ops
  with NonEmptyTraverse.Ops
  with OptionOps with OrderOps with PartialOrderOps
  with Profunctor.Ops with Reducible.Ops with SemigroupOps with Semigroupal.Ops with Show.Ops with Strong.Ops with Traverse.Ops with TraverseFilter.Ops
  with UnorderedFoldable.Ops with UnorderedTraverse.Ops with WriterOps

private trait SemigroupOps {
  given [A] {
  	@alpha("combine") def (a: A) |+| (b: A) given (A: Semigroup[A]): A = A.combine(a, b)
  }
}

private trait MonoidOps {
  given [A] {
    def (a: A) isEmpty given Monoid[A], Eq[A]: Boolean = the[Monoid[A]].isEmpty(a)
  }
}

private trait EqOps {
	given [A] {
	  @alpha("eqv") def (a: A) === (b: A) given (A: Eq[A]): Boolean = A.eqv(a, b)
  }
}

private trait HashOps {
  given [A] {
    def (a: A) hash given (A: Hash[A]): Int = A.hash(a)
  }
}

private trait OrderOps {
  given [A] {
    def (a: A) compare(b: A) given (A: Order[A]): Int = A.compare(a, b)
    def (a: A) min(b: A) given (A: Order[A]): A = A.min(a, b)
    def (a: A) max(b: A) given (A: Order[A]): A = A.max(a, b)
    def (a: A) comparison(b: A) given (A: Order[A]): Comparison = A.comparison(a, b)
  }
}

private trait PartialOrderOps {
  given [A] given (A: PartialOrder[A]) {
    // Currently produce ClassFormatErrors.
    def (a: A) >(b: A): Boolean = A.gt(a, b)
    def (a: A) >=(b: A): Boolean = A.gteqv(a, b)
    def (a: A) <(b: A): Boolean = A.lt(a, b)
    def (a: A) <=(b: A): Boolean = A.lteqv(a, b)

    def (a: A) partialCompare(b: A): Double = A.partialCompare(a, b)
    def (a: A) tryCompare(b: A): Option[Int] = A.tryCompare(a, b)
    def (a: A) pmin(b: A): Option[A] = A.pmin(a, b)
    def (a: A) pmax(b: A): Option[A] = A.pmax(a, b)
  }
}

private trait WriterOps {
	given [A] {
		def (a: A) tell: Writer[A, Unit] = Writer(a, ())
    def (a: A) writer[W](w: W): Writer[W, A] = Writer(w, a)
  }
}

private trait ListOps {
  given [A] {
    /**
     * Returns an Option of NonEmptyList from a List
     *
     * Example:
     * {{{
     * scala> import cats.data.NonEmptyList
     * scala> import cats.implicits._
     *
     * scala> val result1: List[Int] = List(1, 2)
     * scala> result1.toNel
     * res0: Option[NonEmptyList[Int]] = Some(NonEmptyList(1, 2))
     *
     * scala> val result2: List[Int] = List.empty[Int]
     * scala> result2.toNel
     * res1: Option[NonEmptyList[Int]] = None
     * }}}
     */
    def (a: List[A]) toNel: Option[NonEmptyList[A]] = NonEmptyList.fromList(a)

    /**
     * Groups elements inside this `List` according to the `Order` of the keys
     * produced by the given mapping function.
     *
     * {{{
     * scala> import cats.data.NonEmptyList
     * scala> import scala.collection.immutable.SortedMap
     * scala> import cats.implicits._
     *
     * scala> val list = List(12, -2, 3, -5)
     *
     * scala> list.groupByNel(_ >= 0)
     * res0: SortedMap[Boolean, NonEmptyList[Int]] = Map(false -> NonEmptyList(-2, -5), true -> NonEmptyList(12, 3))
     * }}}
     */
    def (a: List[A]) groupByNel[B](f: A => B) given (B: Order[B]): SortedMap[B, NonEmptyList[A]] = {
      given as Ordering[B] = B.toOrdering
      NonEmptyList.fromList(a).fold(SortedMap.empty[B, NonEmptyList[A]])(_.groupBy(f))
    }

    /**
     * Groups elements inside this `List` according to the `Order` of the keys
     * produced by the given mapping function.
     *
     * {{{
     * scala> import cats.data.NonEmptyChain
     * scala> import scala.collection.immutable.SortedMap
     * scala> import cats.implicits._
     *
     * scala> val list = List(12, -2, 3, -5)
     *
     * scala> list.groupByNec(_ >= 0)
     * res0: SortedMap[Boolean, NonEmptyChain[Int]] = Map(false -> Chain(-2, -5), true -> Chain(12, 3))
     * }}}
     */
    def (a: List[A]) groupByNec[B](f: A => B) given (B: Order[B]): SortedMap[B, NonEmptyChain[A]] = {
      given as Ordering[B] = B.toOrdering
      NonEmptyChain.fromSeq(a).fold(SortedMap.empty[B, NonEmptyChain[A]])(_.groupBy(f).toSortedMap)
    }
  }
}