package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Band, BoundedSemilattice, CommutativeGroup, CommutativeMonoid, CommutativeSemigroup, Group, Monoid, Semigroup, Semilattice}

private[kernel]class Function1Semigroup[A, B] given (B: Semigroup[B]) extends Semigroup[A => B] {
  override def combine(x: A => B, y: A => B): A => B =
    (a: A) => B.combine(x(a), y(a))
}

private[kernel] class Function1CommutativeSemigroup[A, B] given (B: CommutativeSemigroup[B]) extends Function1Semigroup[A, B] with CommutativeSemigroup[A => B] 

private[kernel] class Function1Band[A, B] given (B: Band[B]) extends Function1Semigroup[A, B] with Band[A => B]

private[kernel] class Function1Semilattice[A, B] given (B: Semilattice[B]) extends Function1Semigroup[A, B] with Semilattice[A => B]

private[kernel] class Function1Monoid[A, B] given (B: Monoid[B]) extends Function1Semigroup[A, B] with Monoid[A => B] {
  val empty: A => B =
    (_: A) => B.empty
}

private[kernel] class Function1CommutativeMonoid[A, B] given (B: CommutativeMonoid[B]) extends Function1Monoid[A, B] with CommutativeMonoid[A => B]

private[kernel] class Function1BoundedSemilattice[A, B] given (B: BoundedSemilattice[B]) extends Function1Monoid[A, B] with BoundedSemilattice[A => B] 

private[kernel] class Function1Group[A, B] given (B: Group[B]) extends Function1Monoid[A, B] with Group[A => B] {
  def inverse(x: A => B): A => B =
    (a: A) => B.inverse(x(a))
}

private[kernel] class Function1CommutativeGroup[A, B] given (B: CommutativeGroup[B]) extends Function1Group[A, B] with CommutativeGroup[A => B]
