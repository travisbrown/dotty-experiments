package io.circe.cats.kernel.laws

given IsEqOps[A] {
  def (lhs: A) <->(rhs: A): IsEq[A] = IsEq(lhs, rhs)
}