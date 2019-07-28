package io.circe.cats

import io.circe.cats.arrow.Arrow
import io.circe.cats.kernel._
import scala.collection.immutable.{SortedMap, SortedSet}

/**
 * Must obey the laws defined in cats.laws.InvariantLaws.
 */
trait Invariant[F[_]] extends Serializable { self =>

  /**
   * Transform an `F[A]` into an `F[B]` by providing a transformation from `A`
   * to `B` and one from `B` to `A`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import scala.concurrent.duration._
   * scala> val durSemigroup: Semigroup[FiniteDuration] =
   *      | Invariant[Semigroup].imap(Semigroup[Long])(Duration.fromNanos)(_.toNanos)
   * scala> durSemigroup.combine(2.seconds, 3.seconds)
   * res1: FiniteDuration = 5 seconds
   * }}}
   */
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]

  def compose[G[_]] given Invariant[G]: Invariant[[x] =>> F[G[x]]] =
    new ComposedInvariant[F, G] {
      val F = self
      val G = the[Invariant[G]]
    }

  def composeFunctor[G[_]] given Functor[G]: Invariant[[x] =>> F[G[x]]] =
    new ComposedInvariantCovariant[F, G] {
      val F = self
      val G = the[Functor[G]]
    }

  def composeContravariant[G[_]] given Contravariant[G]: Invariant[[x] =>> F[G[x]]] =
    new ComposedInvariantContravariant[F, G] {
      val F = self
      val G = the[Contravariant[G]]
    }
}

object Invariant extends CommutativeMonadInstances {
  def apply[F[_]] given (F: Invariant[F]): Invariant[F] = F

  given as (Distributive[Id] & Comonad[Id]) = io.circe.cats.instances.IdInstance
  given [A] as Comonad[[x] =>> (A, x)] = io.circe.cats.instances.Tuple2Instance[A]

  given as (MonadError[Option, Unit] & Alternative[Option]) = io.circe.cats.instances.OptionInstance
  given as (Monad[List] & Alternative[List] & CoflatMap[List]) = io.circe.cats.instances.ListInstance
  given as (Monad[Vector] & Alternative[Vector] & CoflatMap[Vector]) = io.circe.cats.instances.VectorInstance
  given as (Monad[Stream] & Alternative[Stream] & CoflatMap[Stream]) = io.circe.cats.instances.StreamInstance

  given [A] as MonadError[[x] =>> Either[A, x], A] = io.circe.cats.instances.EitherInstance[A]
  given [I] as (Distributive[[x] =>> I => x] & Monad[[x] =>> I => x]) = io.circe.cats.instances.Function1InstanceR[I]

  given [K] as FlatMap[[x] =>> SortedMap[K, x]] given Order[K] = io.circe.cats.instances.SortedMapInstance[K]

  given as Distributive[Function0] = io.circe.cats.instances.Function0Instance

  given as Contravariant[Hash] {
      /**
       * Derive a `Hash` for `B` given an `Hash[A]` and a function `B => A`.
       */
    def contramap[A, B](A: Hash[A])(f: B => A): Hash[B] = Hash.by(f) given A
  }

  given [R] as Contravariant[[x] =>> x => R] {
    def contramap[A, B](fa: A => R)(f: B => A): B => R =
      fa.compose(f)
  }

  /**
   * Creates an applicative functor for `F`, holding domain fixed and combining
   * over the codomain.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.Applicative.catsApplicativeForArrow
   * scala> val toLong: Int => Long = _.toLong
   * scala> val double: Int => Int = 2*_
   * scala> val f: Int => (Long, Int) = catsApplicativeForArrow.product(toLong, double)
   * scala> f(3)
   * res0: (Long, Int) = (3,6)
   * }}}
   */
  given [F[_, _], A] as Applicative[[x] =>> F[A, x]] given (F: Arrow[F]) =
    new ArrowApplicative[F, A]

  given as Invariant[Monoid] {
    def imap[A, B](fa: Monoid[A])(f: A => B)(g: B => A): Monoid[B] = new Monoid[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.toIterator.map(g)).map(f)
    }
  }

  given as Invariant[Band] {
    def imap[A, B](fa: Band[A])(f: A => B)(g: B => A): Band[B] = new Band[B] {
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.toIterator.map(g)).map(f)
    }
  }

  given as Invariant[Semilattice] {
    def imap[A, B](fa: Semilattice[A])(f: A => B)(g: B => A): Semilattice[B] = new Semilattice[B] {
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.toIterator.map(g)).map(f)
    }
  }

  given as Invariant[CommutativeMonoid] {
    def imap[A, B](fa: CommutativeMonoid[A])(f: A => B)(g: B => A): CommutativeMonoid[B] = new CommutativeMonoid[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.toIterator.map(g)).map(f)
    }
  }

  given as Invariant[BoundedSemilattice] {
    def imap[A, B](fa: BoundedSemilattice[A])(f: A => B)(g: B => A): BoundedSemilattice[B] = new BoundedSemilattice[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.toIterator.map(g)).map(f)
    }
  }

  given as Invariant[Group] {
    def imap[A, B](fa: Group[A])(f: A => B)(g: B => A): Group[B] = new Group[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      def inverse(b: B): B = f(fa.inverse(g(b)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.toIterator.map(g)).map(f)
    }
  }

  given as Invariant[CommutativeGroup] {
    def imap[A, B](fa: CommutativeGroup[A])(f: A => B)(g: B => A): CommutativeGroup[B] = new CommutativeGroup[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      def inverse(b: B): B = f(fa.inverse(g(b)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.toIterator.map(g)).map(f)
    }
  }

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

  given as InvariantMonoidal[Semigroup] {
    def product[A, B](fa: Semigroup[A], fb: Semigroup[B]): Semigroup[(A, B)] = new Semigroup[(A, B)] {
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: Semigroup[A])(f: A => B)(g: B => A): Semigroup[B] = new Semigroup[B] {
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
    }

    def unit: Semigroup[Unit] = the[Semigroup[Unit]]
  }

  given as InvariantMonoidal[CommutativeSemigroup] {
    def product[A, B](fa: CommutativeSemigroup[A], fb: CommutativeSemigroup[B]): CommutativeSemigroup[(A, B)] =
      new CommutativeSemigroup[(A, B)] {
        def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
      }

    def imap[A, B](fa: CommutativeSemigroup[A])(f: A => B)(g: B => A): CommutativeSemigroup[B] =
      new CommutativeSemigroup[B] {
        def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      }

    def unit: CommutativeSemigroup[Unit] = the[CommutativeSemigroup[Unit]]
  }

  private[cats] trait Ops {
    given [F[_], A] given (F: Invariant[F]) {
      def (fa: F[A]) imap[B](f: A => B)(g: B => A): F[B] = F.imap(fa)(f)(g)
    }
  }
}

private class CommutativeMonadInstances {
  given as CommutativeMonad[Id] = io.circe.cats.instances.IdInstance
  given as CommutativeMonad[Option] = io.circe.cats.instances.OptionInstance
}
