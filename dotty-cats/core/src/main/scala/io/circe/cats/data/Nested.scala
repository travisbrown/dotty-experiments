package io.circe.cats.data

import io.circe.cats.{Alternative, Applicative, ApplicativeError, Apply, CommutativeApplicative, CommutativeApply, Contravariant, ContravariantMonoidal, Defer, Distributive, Eval, Foldable, Functor, FunctorFilter, Invariant, InvariantSemigroupal, MonoidK, NonEmptyTraverse, Reducible, SemigroupK, Traverse, TraverseFilter, ~>}
import io.circe.cats.kernel.Eq

/** Similar to [[cats.data.Tuple2K]], but for nested composition.
 *
 * For instance, since both `List` and `Option` have a `Functor`, then so does
 * `List[Option[_]]`. This is represented by this data type via the instantiation
 * `Nested[List, Option, _]`.
 *
 * {{{
 * scala> import cats.Functor
 * scala> import cats.data.Nested
 * scala> import cats.implicits._
 * scala> val listOption: List[Option[Int]] = List(Some(1), None)
 * scala> val f: Int => String = i => (i * 2).toString
 * scala> Functor[List].map(listOption)(opt => opt.map(f))
 * res0: List[Option[String]] = List(Some(2), None)
 * scala> val nested: Nested[List, Option, Int] = Nested(listOption)
 * scala> val result: Nested[List, Option, String] = Functor[Nested[List, Option, _]].map(nested)(f)
 * scala> result.value
 * res1: List[Option[String]] = List(Some(2), None)
 * }}}
 */
final case class Nested[F[_], G[_], A](value: F[G[A]]) {

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[H[_]](f: F ~> H): Nested[H, G, A] =
    Nested(f(value))

}

object Nested extends NestedInstances

sealed abstract private[data] class NestedInstances extends NestedInstances0 {
  implicit def catsDataEqForNested[F[_], G[_], A] given (FGA: Eq[F[G[A]]]): Eq[Nested[F, G, A]] =
    Eq.by[Nested[F, G, A], F[G[A]]](_.value)

  implicit def catsDataNonEmptyTraverseForNested[F[_]: NonEmptyTraverse, G[_]: NonEmptyTraverse]
    : NonEmptyTraverse[[a] =>> Nested[F, G, a]] =
    new NestedNonEmptyTraverse[F, G] {
      val FG: NonEmptyTraverse[[α] =>> F[G[α]]] = NonEmptyTraverse[F].compose[G]
    }

  implicit def catsDataContravariantMonoidalForApplicativeForNested[F[_]: Applicative, G[_]: ContravariantMonoidal]
    : ContravariantMonoidal[[a] =>> Nested[F, G, a]] =
    new NestedContravariantMonoidal[F, G] with NestedContravariant[F, G] {
      val FG: ContravariantMonoidal[[α] =>> F[G[α]]] = Applicative[F].composeContravariantMonoidal[G]
    }

  implicit def catsDataDeferForNested[F[_], G[_]] given (F: Defer[F]): Defer[[a] =>> Nested[F, G, a]] =
    new Defer[[a] =>> Nested[F, G, a]] {
      def defer[A](fa: => Nested[F, G, A]): Nested[F, G, A] =
        Nested(F.defer(fa.value))
    }

  implicit def catsDataTraverseFilterForNested[F[_], G[_]] given (F0: Traverse[F],
                                                           G0: TraverseFilter[G]): TraverseFilter[[a] =>> Nested[F, G, a]] =
    new NestedTraverseFilter[F, G] {
      implicit val F: Traverse[F] = F0
      implicit val G: TraverseFilter[G] = G0
    }
}

sealed abstract private[data] class NestedInstances0 extends NestedInstances1 {
  implicit def catsDataTraverseForNested[F[_]: Traverse, G[_]: Traverse]: Traverse[[a] =>> Nested[F, G, a]] =
    new NestedTraverse[F, G] {
      val FG: Traverse[[α] =>> F[G[α]]] = Traverse[F].compose[G]
    }

  implicit def catsDataFunctorFilterForNested[F[_], G[_]] given (F0: Functor[F],
                                                          G0: FunctorFilter[G]): FunctorFilter[[a] =>> Nested[F, G, a]] =
    new NestedFunctorFilter[F, G] {
      implicit val F: Functor[F] = F0
      implicit val G: FunctorFilter[G] = G0
    }
}

sealed abstract private[data] class NestedInstances1 extends NestedInstances2 {
  implicit def catsDataReducibleForNested[F[_]: Reducible, G[_]: Reducible]: Reducible[[a] =>> Nested[F, G, a]] =
    new NestedReducible[F, G] {
      val FG: Reducible[[α] =>> F[G[α]]] = Reducible[F].compose[G]
    }

  implicit def catsDataFunctorForContravariantForNested[F[_]: Contravariant, G[_]: Contravariant]
    : Functor[[a] =>> Nested[F, G, a]] =
    new NestedFunctor[F, G] {
      val FG: Functor[[α] =>> F[G[α]]] = Contravariant[F].compose[G]
    }
}

sealed abstract private[data] class NestedInstances2 extends NestedInstances3 {
  implicit def catsDataFoldableForNested[F[_]: Foldable, G[_]: Foldable]: Foldable[[a] =>> Nested[F, G, a]] =
    new NestedFoldable[F, G] {
      val FG: Foldable[[α] =>> F[G[α]]] = Foldable[F].compose[G]
    }

  implicit def catsDataContravariantForCovariantNested[F[_]: Contravariant, G[_]: Functor]
    : Contravariant[[a] =>> Nested[F, G, a]] =
    new NestedContravariant[F, G] {
      val FG: Contravariant[[α] =>> F[G[α]]] = Contravariant[F].composeFunctor[G]
    }
}

sealed abstract private[data] class NestedInstances3 extends NestedInstances4 {
  implicit def catsDataAlternativeForNested[F[_]: Alternative, G[_]: Applicative]: Alternative[[a] =>> Nested[F, G, a]] =
    new NestedAlternative[F, G] {
      val FG: Alternative[[α] =>> F[G[α]]] = Alternative[F].compose[G]
    }

  implicit def catsDataContravariantForContravariantNested[F[_]: Functor, G[_]: Contravariant]
    : Contravariant[[a] =>> Nested[F, G, a]] =
    new NestedContravariant[F, G] {
      val FG: Contravariant[[α] =>> F[G[α]]] = Functor[F].composeContravariant[G]
    }
}

sealed abstract private[data] class NestedInstances4 extends NestedInstances5 {
  /*implicit def catsDataApplicativeErrorForNested[F[_]: ApplicativeError[*[_], E], G[_]: Applicative, E]
    : ApplicativeError[[a[_]] => Nested[F, G, a], E] =
    new NestedApplicativeError[F, G, E] {
      val G: Applicative[G] = Applicative[G]

      val AEF: ApplicativeError[F, E] = ApplicativeError[F, E]
    }*/

}

sealed abstract private[data] class NestedInstances5 extends NestedInstances6 {
  implicit def catsDataCommutativeApplicativeForNestedContravariant[F[_]: CommutativeApplicative, G[_]: CommutativeApplicative]
    : CommutativeApplicative[[a] =>> Nested[F, G, a]] =
    new NestedApplicative[F, G] with CommutativeApplicative[[a] =>> Nested[F, G, a]] {
      val FG: Applicative[[α] =>> F[G[α]]] = Applicative[F].compose[G]
    }

  implicit def catsDataMonoidKForNested[F[_]: MonoidK, G[_]]: MonoidK[[a] =>> Nested[F, G, a]] =
    new NestedMonoidK[F, G] {
      val FG: MonoidK[[α] =>> F[G[α]]] = MonoidK[F].compose[G]
    }
}

sealed abstract private[data] class NestedInstances6 extends NestedInstances7 {
  implicit def catsDataCommutativeApplyForNestedContravariant[F[_]: CommutativeApply, G[_]: CommutativeApply]
    : CommutativeApply[[a] =>> Nested[F, G, a]] =
    new NestedApply[F, G] with CommutativeApply[[a] =>> Nested[F, G, a]] {
      val FG: Apply[[α] =>> F[G[α]]] = Apply[F].compose[G]
    }

  implicit def catsDataSemigroupKForNested[F[_]: SemigroupK, G[_]]: SemigroupK[[a] =>> Nested[F, G, a]] =
    new NestedSemigroupK[F, G] {
      val FG: SemigroupK[[α] =>> F[G[α]]] = SemigroupK[F].compose[G]
    }
}

sealed abstract private[data] class NestedInstances7 extends NestedInstances8 {
  implicit def catsDataApplicativeForNested[F[_]: Applicative, G[_]: Applicative]: Applicative[[a] =>> Nested[F, G, a]] =
    new NestedApplicative[F, G] {
      val FG: Applicative[[α] =>> F[G[α]]] = Applicative[F].compose[G]
    }
}

sealed abstract private[data] class NestedInstances8 extends NestedInstances9 {
  implicit def catsDataApplyForNested[F[_]: Apply, G[_]: Apply]: Apply[[a] =>> Nested[F, G, a]] =
    new NestedApply[F, G] {
      val FG: Apply[[α] =>> F[G[α]]] = Apply[F].compose[G]
    }

  implicit def catsDataDistributiveForNested[F[_]: Distributive, G[_]: Distributive]: Distributive[[a] =>> Nested[F, G, a]] =
    new NestedDistributive[F, G] {
      val FG: Distributive[[α] =>> F[G[α]]] = Distributive[F].compose[G]
    }
}

sealed abstract private[data] class NestedInstances9 extends NestedInstances10 {
  implicit def catsDataInvariantSemigroupalApplyForNested[F[_]: InvariantSemigroupal, G[_]: Apply]
    : InvariantSemigroupal[[a] =>> Nested[F, G, a]] =
    new NestedInvariantSemigroupalApply[F, G] {
      val FG: InvariantSemigroupal[[α] =>> F[G[α]]] = InvariantSemigroupal[F].composeApply[G]
    }
}

sealed abstract private[data] class NestedInstances10 extends NestedInstances11 {
  implicit def catsDataFunctorForNested[F[_]: Functor, G[_]: Functor]: Functor[[a] =>> Nested[F, G, a]] =
    new NestedFunctor[F, G] {
      val FG: Functor[[α] =>> F[G[α]]] = Functor[F].compose[G]
    }
}

sealed abstract private[data] class NestedInstances11 extends NestedInstances12 {
  implicit def catsDataInvariantForNested[F[_]: Invariant, G[_]: Invariant]: Invariant[[a] =>> Nested[F, G, a]] =
    new NestedInvariant[F, G] {
      val FG: Invariant[[α] =>> F[G[α]]] = Invariant[F].compose[G]
    }
}

sealed abstract private[data] class NestedInstances12 extends NestedInstances13 {
  implicit def catsDataInvariantForCovariantNested[F[_]: Invariant, G[_]: Functor]: Invariant[[a] =>> Nested[F, G, a]] =
    new NestedInvariant[F, G] {
      val FG: Invariant[[α] =>> F[G[α]]] = Invariant[F].composeFunctor[G]
    }
}

sealed abstract private[data] class NestedInstances13 {
  implicit def catsDataInvariantForNestedContravariant[F[_]: Invariant, G[_]: Contravariant]
    : Invariant[[a] =>> Nested[F, G, a]] =
    new NestedInvariant[F, G] {
      val FG: Invariant[[α] =>> F[G[α]]] = Invariant[F].composeContravariant[G]
    }
}

private[data] trait NestedInvariant[F[_], G[_]] extends Invariant[[a] =>> Nested[F, G, a]] {
  def FG: Invariant[[α] =>> F[G[α]]]

  override def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
    Nested(FG.imap(fga.value)(f)(g))
}

private[data] trait NestedFunctor[F[_], G[_]] extends Functor[[a] =>> Nested[F, G, a]] with NestedInvariant[F, G] {
  override def FG: Functor[[α] =>> F[G[α]]]

  override def map[A, B](fga: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
    Nested(FG.map(fga.value)(f))
}

private[data] trait NestedApply[F[_], G[_]] extends Apply[[a] =>> Nested[F, G, a]] with NestedFunctor[F, G] {
  override def FG: Apply[[α] =>> F[G[α]]]

  override def ap[A, B](fgf: Nested[F, G, A => B])(fga: Nested[F, G, A]): Nested[F, G, B] =
    Nested(FG.ap(fgf.value)(fga.value))

  override def product[A, B](fga: Nested[F, G, A], fgb: Nested[F, G, B]): Nested[F, G, (A, B)] =
    Nested(FG.product(fga.value, fgb.value))
}

private[data] trait NestedApplicative[F[_], G[_]] extends Applicative[[a] =>> Nested[F, G, a]] with NestedApply[F, G] {
  def FG: Applicative[[α] =>> F[G[α]]]

  def pure[A](x: A): Nested[F, G, A] = Nested(FG.pure(x))
}

abstract private[data] class NestedApplicativeError[F[_], G[_], E]
    extends ApplicativeError[[a] =>> Nested[F, G, a], E]
    with NestedApplicative[F, G] {
  def G: Applicative[G]
  def AEF: ApplicativeError[F, E]

  def FG: Applicative[[α] =>> F[G[α]]] = AEF.compose[G](G)

  def raiseError[A](e: E): Nested[F, G, A] = Nested(AEF.map(AEF.raiseError[A](e))(G.pure))

  def handleErrorWith[A](fa: Nested[F, G, A])(f: E => Nested[F, G, A]): Nested[F, G, A] =
    Nested(AEF.handleErrorWith(fa.value)(f.andThen(_.value)))

}

private[data] trait NestedSemigroupK[F[_], G[_]] extends SemigroupK[[a] =>> Nested[F, G, a]] {
  def FG: SemigroupK[[α] =>> F[G[α]]]

  def combineK[A](x: Nested[F, G, A], y: Nested[F, G, A]): Nested[F, G, A] = Nested(FG.combineK(x.value, y.value))
}

private[data] trait NestedMonoidK[F[_], G[_]] extends MonoidK[[a] =>> Nested[F, G, a]] with NestedSemigroupK[F, G] {
  def FG: MonoidK[[α] =>> F[G[α]]]

  def empty[A]: Nested[F, G, A] = Nested(FG.empty[A])
}

private[data] trait NestedAlternative[F[_], G[_]]
    extends Alternative[[a] =>> Nested[F, G, a]]
    with NestedApplicative[F, G]
    with NestedMonoidK[F, G] {
  def FG: Alternative[[α] =>> F[G[α]]]
}

private[data] trait NestedFoldable[F[_], G[_]] extends Foldable[[a] =>> Nested[F, G, a]] {
  def FG: Foldable[[α] =>> F[G[α]]]

  def foldLeft[A, B](fga: Nested[F, G, A], b: B)(f: (B, A) => B): B =
    FG.foldLeft(fga.value, b)(f)

  def foldRight[A, B](fga: Nested[F, G, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    FG.foldRight(fga.value, lb)(f)
}

private[data] trait NestedTraverse[F[_], G[_]]
    extends Traverse[[a] =>> Nested[F, G, a]]
    with NestedFoldable[F, G]
    with NestedFunctor[F, G] {
  def FG: Traverse[[α] =>> F[G[α]]]

  override def traverse[H[_]: Applicative, A, B](fga: Nested[F, G, A])(f: A => H[B]): H[Nested[F, G, B]] =
    Applicative[H].map(FG.traverse(fga.value)(f))(Nested(_))
}

private[data] trait NestedDistributive[F[_], G[_]] extends Distributive[[a] =>> Nested[F, G, a]] with NestedFunctor[F, G] {
  def FG: Distributive[[α] =>> F[G[α]]]

  def distribute[H[_]: Functor, A, B](ha: H[A])(f: A => Nested[F, G, B]): Nested[F, G, H[B]] =
    Nested(FG.distribute(ha) { a =>
      f(a).value
    })
}

private[data] trait NestedReducible[F[_], G[_]] extends Reducible[[a] =>> Nested[F, G, a]] with NestedFoldable[F, G] {
  def FG: Reducible[[α] =>> F[G[α]]]

  def reduceLeftTo[A, B](fga: Nested[F, G, A])(f: A => B)(g: (B, A) => B): B =
    FG.reduceLeftTo(fga.value)(f)(g)

  def reduceRightTo[A, B](fga: Nested[F, G, A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
    FG.reduceRightTo(fga.value)(f)(g)
}

private[data] trait NestedNonEmptyTraverse[F[_], G[_]]
    extends NonEmptyTraverse[[a] =>> Nested[F, G, a]]
    with NestedTraverse[F, G]
    with NestedReducible[F, G] {
  def FG: NonEmptyTraverse[[α] =>> F[G[α]]]

  override def nonEmptyTraverse[H[_]: Apply, A, B](fga: Nested[F, G, A])(f: A => H[B]): H[Nested[F, G, B]] =
    Apply[H].map(FG.nonEmptyTraverse(fga.value)(f))(Nested(_))
}

private[data] trait NestedContravariant[F[_], G[_]] extends Contravariant[[a] =>> Nested[F, G, a]] {
  def FG: Contravariant[[α] =>> F[G[α]]]

  override def contramap[A, B](fga: Nested[F, G, A])(f: B => A): Nested[F, G, B] =
    Nested(FG.contramap(fga.value)(f))
}

private[data] trait NestedContravariantMonoidal[F[_], G[_]] extends ContravariantMonoidal[[a] =>> Nested[F, G, a]] {
  def FG: ContravariantMonoidal[[α] =>> F[G[α]]]

  def unit: Nested[F, G, Unit] = Nested(FG.unit)

  def contramap[A, B](fa: Nested[F, G, A])(f: B => A): Nested[F, G, B] =
    Nested(FG.contramap(fa.value)(f))

  def product[A, B](fa: Nested[F, G, A], fb: Nested[F, G, B]): Nested[F, G, (A, B)] =
    Nested(FG.product(fa.value, fb.value))
}

private[data] trait NestedInvariantSemigroupalApply[F[_], G[_]] extends InvariantSemigroupal[[a] =>> Nested[F, G, a]] {
  def FG: InvariantSemigroupal[[α] =>> F[G[α]]]

  def imap[A, B](fa: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
    Nested(FG.imap(fa.value)(f)(g))

  def product[A, B](fa: Nested[F, G, A], fb: Nested[F, G, B]): Nested[F, G, (A, B)] =
    Nested(FG.product(fa.value, fb.value))
}

abstract private[data] class NestedFunctorFilter[F[_], G[_]] extends FunctorFilter[[a] =>> Nested[F, G, a]] {
  implicit val F: Functor[F]

  implicit val G: FunctorFilter[G]

  def functor: Functor[[a] =>> Nested[F, G, a]] = Nested.catsDataFunctorForNested(F, G.functor)

  def mapFilter[A, B](fa: Nested[F, G, A])(f: (A) => Option[B]): Nested[F, G, B] =
    Nested[F, G, B](F.map(fa.value)(G.mapFilter(_)(f)))

  override def collect[A, B](fa: Nested[F, G, A])(f: PartialFunction[A, B]): Nested[F, G, B] =
    Nested[F, G, B](F.map(fa.value)(G.collect(_)(f)))

  override def flattenOption[A](fa: Nested[F, G, Option[A]]): Nested[F, G, A] =
    Nested[F, G, A](F.map(fa.value)(G.flattenOption))

  override def filter[A](fa: Nested[F, G, A])(f: (A) => Boolean): Nested[F, G, A] =
    Nested[F, G, A](F.map(fa.value)(G.filter(_)(f)))
}

abstract private[data] class NestedTraverseFilter[F[_], G[_]]
    extends NestedFunctorFilter[F, G]
    with TraverseFilter[[a] =>> Nested[F, G, a]] {
  implicit val F: Traverse[F]

  implicit val G: TraverseFilter[G]

  def traverse: Traverse[[a] =>> Nested[F, G, a]] = Nested.catsDataTraverseForNested(F, G.traverse)

  override def filterA[H[_], A](
    fa: Nested[F, G, A]
  )(f: A => H[Boolean]) given (H: Applicative[H]): H[Nested[F, G, A]] =
    H.map(F.traverse(fa.value)(G.filterA[H, A](_)(f)))(Nested[F, G, A])

  def traverseFilter[H[_], A, B](
    fga: Nested[F, G, A]
  )(f: A => H[Option[B]]) given (H: Applicative[H]): H[Nested[F, G, B]] =
    H.map(F.traverse[H, G[A], G[B]](fga.value)(ga => G.traverseFilter(ga)(f)))(Nested[F, G, B])
}
