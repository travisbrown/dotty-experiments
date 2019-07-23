package io.circe.cats.data

import io.circe.cats.arrow.{Category, Compose}
import io.circe.cats.kernel.Eq

/**
 * The dual category of some other category, `Arr`.
 */
case class Op[Arr[_, _], A, B](run: Arr[B, A]) {
  def compose[Z](op: Op[Arr, Z, A]) given (Arr: Compose[Arr]): Op[Arr, Z, B] =
    Op(Arr.compose(op.run, run))

  def eqv(op: Op[Arr, A, B]) given (Arr: Eq[Arr[B, A]]): Boolean =
    Arr.eqv(run, op.run)
}

object Op extends OpInstances0 {
  given [Arr[_, _]] as Category[[x, y] =>> Op[Arr, x, y]] given (ArrC: Category[Arr])  =
    new OpCategory[Arr] with OpCompose[Arr]

  given [Arr[_, _], A, B] as Eq[Op[Arr, A, B]] given (ArrEq: Eq[Arr[B, A]]) =
    new OpEq[Arr, A, B] {}

  private[this] trait OpCategory[Arr[_, _]] given (Arr: Category[Arr]) extends Category[[x, y] =>> Op[Arr, x, y]] with OpCompose[Arr] {
    override def id[A]: Op[Arr, A, A] = Op(Arr.id)
  }

  private[this] trait OpEq[Arr[_, _], A, B] given Eq[Arr[B, A]] extends Eq[Op[Arr, A, B]] {
    def eqv(f: Op[Arr, A, B], g: Op[Arr, A, B]): Boolean =
      f.eqv(g)
  }
}

sealed abstract private[data] class OpInstances0 {
  protected[this] trait OpCompose[Arr[_, _]] given Compose[Arr] extends Compose[[x, y] =>> Op[Arr, x, y]] {
    def compose[A, B, C](f: Op[Arr, B, C], g: Op[Arr, A, B]): Op[Arr, A, C] =
      f.compose(g)
  }

  given [Arr[_, _]] as Compose[[x, y] =>> Op[Arr, x, y]] given Compose[Arr] =
    new OpCompose[Arr] {}
}
