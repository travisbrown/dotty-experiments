package io.circe

import io.circe.cats.kernel.{Monoid, Semigroup}

/**
 * Symbolic aliases for various types are defined here.
 */
package object cats {

  type ~>[F[_], G[_]] = io.circe.cats.arrow.FunctionK[F, G]

  type ⊥ = Nothing
  type ⊤ = Any

  /** [[cats.InjectK]][F, G] */
  type :<:[F[_], G[_]] = InjectK[F, G]

  /** [[cats.InjectK]][F, G] */
  type :≺:[F[_], G[_]] = InjectK[F, G]

  /**
   * Identity, encoded as `type Id[A] = A`, a convenient alias to make
   * identity instances well-kinded.
   *
   * The identity monad can be seen as the ambient monad that encodes
   * the effect of having no effect. It is ambient in the sense that
   * plain pure values are values of `Id`.
   *
   * For instance, the [[cats.Functor]] instance for `[[cats.Id]]`
   * allows us to apply a function `A => B` to an `Id[A]` and get an
   * `Id[B]`. However, an `Id[A]` is the same as `A`, so all we're doing
   * is applying a pure function of type `A => B` to a pure value  of
   * type `A` to get a pure value of type `B`. That is, the instance
   * encodes pure unary function application.
   */
  type Id[A] = A
  type Endo[A] = A => A

  //given as Bimonad[Id] = IdInstance
  //given as CommutativeMonad[Id] & Comonad[Id] = IdInstance
  //given as Comonad[Id] = IdInstance
  //given as NonEmptyTraverse[Id] = IdInstance
  //given as Distributive[Id] = IdInstance

  /**
   * Witness for: Id[A] <-> Unit => A
   */
  given as Representable.Aux[Id, Unit] = new Representable[Id] {
    override type Representation = Unit
    override val F: Functor[Id] = Functor[Id]

    override def tabulate[A](f: Unit => A): Id[A] = f(())

    override def index[A](f: Id[A]): Unit => A = (_: Unit) => f
  }

  given as Parallel[Id, Id] = Parallel.identity

  /*type Eq[A] = cats.kernel.Eq[A]
  type PartialOrder[A] = cats.kernel.PartialOrder[A]
  type Comparison = cats.kernel.Comparison
  type Order[A] = cats.kernel.Order[A]
  type Hash[A] = cats.kernel.Hash[A]
  type Semigroup[A] = cats.kernel.Semigroup[A]
  type Monoid[A] = cats.kernel.Monoid[A]
  type Group[A] = cats.kernel.Group[A]

  val Eq = cats.kernel.Eq
  val PartialOrder = cats.kernel.PartialOrder
  val Order = cats.kernel.Order
  val Comparison = cats.kernel.Comparison
  val Hash = cats.kernel.Hash
  val Semigroup = cats.kernel.Semigroup
  val Monoid = cats.kernel.Monoid
  val Group = cats.kernel.Group*/
}
