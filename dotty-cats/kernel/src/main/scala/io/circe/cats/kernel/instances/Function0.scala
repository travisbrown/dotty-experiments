package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Band, BoundedSemilattice, CommutativeGroup, CommutativeMonoid, CommutativeSemigroup, Eq, Group, Hash, Monoid, Order, PartialOrder, Semigroup, Semilattice}

private[kernel] class Function0Eq[A] given (A: Eq[A]) extends Eq[() => A] {
  def eqv(x: () => A, y: () => A): Boolean = A.eqv(x(), y())
}

private[kernel] class Function0Hash[A] given (A: Hash[A]) extends Function0Eq[A] with Hash[() => A] {
  def hash(x: () => A) = A.hash(x())
}

private[kernel] class Function0PartialOrder[A] given (A: PartialOrder[A]) extends PartialOrder[() => A] {
  override def eqv(x: () => A, y: () => A): Boolean = A.eqv(x(), y())
  def partialCompare(x: () => A, y: () => A): Double = A.partialCompare(x(), y())
}

private[kernel] class Function0Order[A] given (A: Order[A]) extends Order[() => A] {
  override def eqv(x: () => A, y: () => A): Boolean = A.eqv(x(), y())
  def compare(x: () => A, y: () => A): Int = A.compare(x(), y())
}

private[kernel] class Function0Semigroup[A] given (A: Semigroup[A]) extends Semigroup[() => A] {
  override def combine(x: () => A, y: () => A): () => A =
    () => A.combine(x(), y())
}

private[kernel] class Function0CommutativeSemigroup[A] given CommutativeSemigroup[A] extends Function0Semigroup[A] with CommutativeSemigroup[() => A]

private[kernel] class Function0Band[A] given Band[A] extends Function0Semigroup[A] with Band[() => A]

private[kernel] class Function0Semilattice[A] given Semilattice[A] extends Function0Semigroup[A] with Semilattice[() => A]

private[kernel] class Function0Monoid[A] given (A: Monoid[A]) extends Function0Semigroup[A] with Monoid[() => A] {
  val empty: () => A =
    () => A.empty
}

private[kernel] class Function0CommutativeMonoid[A] given CommutativeMonoid[A] extends Function0Monoid[A] with CommutativeMonoid[() => A]

private[kernel] class Function0BoundedSemilattice[A] given BoundedSemilattice[A] extends Function0Monoid[A] with BoundedSemilattice[() => A]

private[kernel] class Function0Group[A] given (A: Group[A]) extends Function0Monoid[A] with Group[() => A] {
  def inverse(x: () => A): () => A =
    () => A.inverse(x())
}

private[kernel] class Function0CommutativeGroup[A] given CommutativeGroup[A] extends Function0Group[A] with CommutativeGroup[() => A]
