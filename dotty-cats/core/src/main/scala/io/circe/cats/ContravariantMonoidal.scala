package io.circe.cats

import io.circe.cats.kernel.{Eq, Monoid, Order, PartialOrder}

/**
 * [[ContravariantMonoidal]] functors are functors that supply
 * a unit along the diagonal map for the `contramap2` operation.
 *
 * Must obey the laws defined in cats.laws.ContravariantMonoidalLaws.
 *
 * Based on ekmett's contravariant library:
 * https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html
 */
trait ContravariantMonoidal[F[_]] extends ContravariantSemigroupal[F] with InvariantMonoidal[F] {

  /**
   * `trivial` produces an instance of `F` for any type `A`
   * that is trivial with respect to `contramap2` along
   * the diagonal
   */
  def trivial[A]: F[A] = contramap(unit)(_ => ())
}

object ContravariantMonoidal {
  def apply[F[_]] given (F: ContravariantMonoidal[F]): ContravariantMonoidal[F] = F

  def monoid[F[_], A] given (F: ContravariantMonoidal[F]): Monoid[F[A]] =
    new ContravariantMonoidalMonoid[F, A]

  given [R] as ContravariantMonoidal[[x] =>> x => R] given (R: Monoid[R]) {
    def unit: Unit => R = Function.const(R.empty)
    def contramap[A, B](fa: A => R)(f: B => A): B => R =
      fa.compose(f)
    def product[A, B](fa: A => R, fb: B => R): ((A, B)) => R =
      (ab: (A, B)) =>
        ab match {
          case (a, b) => R.combine(fa(a), fb(b))
        }
  }

  given as ContravariantMonoidal[Eq] {
    /**
     * Defaults to the trivial equivalence relation
     * contracting the type to a point
     */
    def unit: Eq[Unit] = Eq.allEqual

    /** Derive an `Eq` for `B` given an `Eq[A]` and a function `B => A`.
     *
     * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
     */
    def contramap[A, B](fa: Eq[A])(f: B => A): Eq[B] =
      Eq.by(f) given fa

    def product[A, B](fa: Eq[A], fb: Eq[B]): Eq[(A, B)] =
      Eq.instance { (left, right) =>
        fa.eqv(left._1, right._1) && fb.eqv(left._2, right._2)
      }
  }

  given as ContravariantMonoidal[Equiv] {

      /**
       * Defaults to trivially contracting the type
       * to a point
       */
      def unit: Equiv[Unit] = new Equiv[Unit] {
        def equiv(x: Unit, y: Unit): Boolean = true
      }

      /** Derive an `Equiv` for `B` given an `Equiv[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Equiv[A])(f: B => A): Equiv[B] =
        new Equiv[B] {
          def equiv(l: B, r: B): Boolean =
            fa.equiv(f(l), f(r))
        }

      def product[A, B](fa: Equiv[A], fb: Equiv[B]): Equiv[(A, B)] =
        new Equiv[(A, B)] {
          def equiv(l: (A, B), r: (A, B)): Boolean =
            fa.equiv(l._1, r._1) && fb.equiv(l._2, r._2)
        }
    }

  given as ContravariantMonoidal[Order] {     /**
       * Provides trivial order
       */
      def unit: Order[Unit] = the[Order[Unit]]

      /** Derive an `Order` for `B` given an `Order[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Order[A])(f: B => A): Order[B] =
        Order.by(f) given fa

      def product[A, B](fa: Order[A], fb: Order[B]): Order[(A, B)] =
        new Order[(A, B)] {
          def compare(x: (A, B), y: (A, B)): Int = {
            val z = fa.compare(x._1, y._1)
            if (z == 0) fb.compare(x._2, y._2) else z
          }
        }
    }

  given as ContravariantMonoidal[Ordering] {
      /**
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def unit: Ordering[Unit] = the[Order[Unit]].toOrdering

      def contramap[A, B](fa: Ordering[A])(f: B => A): Ordering[B] = fa.on(f)

      def product[A, B](fa: Ordering[A], fb: Ordering[B]): Ordering[(A, B)] =
        new Ordering[(A, B)] {
          def compare(x: (A, B), y: (A, B)): Int = {
            val z = fa.compare(x._1, y._1)
            if (z == 0) fb.compare(x._2, y._2) else z
          }
        }
    }

  given as ContravariantMonoidal[PartialOrder] {

      /** Derive a `PartialOrder` for `B` given a `PartialOrder[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def unit: PartialOrder[Unit] = the[Order[Unit]]

      def contramap[A, B](fa: PartialOrder[A])(f: B => A): PartialOrder[B] = PartialOrder.by[B, A](f) given fa

      def product[A, B](fa: PartialOrder[A], fb: PartialOrder[B]): PartialOrder[(A, B)] =
        new PartialOrder[(A, B)] {
          def partialCompare(x: (A, B), y: (A, B)): Double = {
            val z = fa.partialCompare(x._1, y._1)
            if (z == 0.0) fb.partialCompare(x._2, y._2) else z
          }
        }
    }

  given as ContravariantMonoidal[PartialOrdering] {
      /** Derive a `PartialOrdering` for `B` given a `PartialOrdering[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def unit: PartialOrdering[Unit] = the[Order[Unit]].toOrdering

      def contramap[A, B](fa: PartialOrdering[A])(f: B => A): PartialOrdering[B] =
        new PartialOrdering[B] {
          def lteq(x: B, y: B): Boolean = fa.lteq(f(x), f(y))
          def tryCompare(x: B, y: B): Option[Int] = fa.tryCompare(f(x), f(y))
        }

      def product[A, B](fa: PartialOrdering[A], fb: PartialOrdering[B]): PartialOrdering[(A, B)] =
        new PartialOrdering[(A, B)] {
          def lteq(x: (A, B), y: (A, B)): Boolean =
            tryCompare(x, y).exists(_ <= 0)
          def tryCompare(x: (A, B), y: (A, B)): Option[Int] =
            fa.tryCompare(x._1, y._1) match {
              case Some(0) => fb.tryCompare(x._2, y._2)
              case option  => option
            }
        }
    }
}

private[cats] class ContravariantMonoidalMonoid[F[_], A] given (F: ContravariantMonoidal[F])
    extends ContravariantSemigroupalSemigroup[F, A]
    with Monoid[F[A]] {
  def empty: F[A] = F.trivial
}
