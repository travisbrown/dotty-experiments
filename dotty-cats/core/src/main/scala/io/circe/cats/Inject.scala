package io.circe.cats

/**
 * Inject is a type class providing an injection from type `A` into
 * type `B`. An injection is a function `inj` which does not destroy
 * any information: for every `b: B` there is at most one `a: A` such
 * that `inj(a) = b`.
 *
 * Because of this all injections admit partial inverses `prj` which
 * pair a value `b: B` back with a single value `a: A`.
 *
 * @since 1.0
 * @note Prior to cats 1.0, Inject handled injection for type
 * constructors. For injection of type constructors, use [[InjectK]].
 *
 * @see [[InjectK]] for injection for [[cats.data.EitherK]]
 */
abstract class Inject[A, B] {
  def inj: A => B

  def prj: B => Option[A]

  final def apply(a: A): B = inj(a)

  final def unapply(b: B): Option[A] = prj(b)
}

object Inject {
  def apply[A, B] given (A: Inject[A, B]): Inject[A, B] = A

  given [A] as Inject[A, A] {
    val inj = identity(_: A)

    val prj = Some(_: A)
  }

  given [A, B] as Inject[A, Either[A, B]] {
    val inj = Left(_: A)

    val prj = (_: Either[A, B]).left.toOption
  }

  given InjectRight[A, B, C] as Inject[A, Either[C, B]] given (I: Inject[A, B]) {
    val inj = (a: A) => Right(I.inj(a))

    val prj = (_: Either[C, B]).toOption.flatMap(I.prj)
  }
}
