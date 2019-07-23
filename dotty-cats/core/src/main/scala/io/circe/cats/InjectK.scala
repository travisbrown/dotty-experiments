package io.circe.cats

import io.circe.cats.arrow.FunctionK
import io.circe.cats.data.EitherK

/**
 * InjectK is a type class providing an injection from type
 * constructor `F` into type constructor `G`. An injection is a
 * functor transformation `inj` which does not destroy any
 * information: for every `ga: G[A]` there is at most one `fa: F[A]`
 * such that `inj(fa) = ga`.
 *
 * Because of this all injections admit partial inverses `prj` which
 * pair a value `ga: G[A]` back with a single value `fa: F[A]`.
 *
 * The behavior of the default instances for the InjectK type class
 * are described thoroughly in "Data types a la carte" (Swierstra
 * 2008).
 *
 * @note Prior to cats 1.0, InjectK was known as [[Inject]].
 *
 * @see [[http://www.staff.science.uu.nl/~swier004/publications/2008-jfp.pdf]]
 * @see [[Inject]] for injection for `Either`
 */
abstract class InjectK[F[_], G[_]] {
  def inj: FunctionK[F, G]

  def prj: FunctionK[G, [x] =>> Option[F[x]]]

  final def apply[A](fa: F[A]): G[A] = inj(fa)

  final def unapply[A](ga: G[A]): Option[F[A]] = prj(ga)
}

object InjectK {
  def apply[F[_], G[_]] given (F: InjectK[F, G]): InjectK[F, G] = F

  given [F[_]] as InjectK[F, F] {
    val inj = FunctionK.id[F]

    val prj = new FunctionK[F, [x] =>> Option[F[x]]] {
      def apply[X](x: F[X]): Option[F[X]] = Some(x)
    }
  }

  given [F[_], G[_]] as InjectK[F, [x] =>> EitherK[F, G, x]] {
    val inj = new FunctionK[F, [x] =>> EitherK[F, G, x]] {
      def apply[X](x: F[X]) = EitherK.leftc(x)
    }

    val prj = new FunctionK[[x] =>> EitherK[F, G, x], [x] =>> Option[F[x]]] {
      def apply[X](x: EitherK[F, G, X]) = x.run.left.toOption
    }
  }

  given InjectKRight[F[_], G[_], H[_]] as InjectK[F, [x] =>> EitherK[H, G, x]] given (F: InjectK[F, G]) {
    val inj = new FunctionK[F, [x] =>> EitherK[H, G, x]] {
      def apply[X](x: F[X]) = EitherK.rightc(F.inj(x))
    }

    val prj = new FunctionK[[x] =>> EitherK[H, G, x], [x] =>> Option[F[x]]] {
      def apply[X](x: EitherK[H, G, X]) = x.run.toOption.flatMap(F.prj(_))
    }
  }
}
