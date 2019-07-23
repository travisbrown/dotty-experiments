package io.circe.cats.kernel.laws

def (lhs: A) <->[A](rhs: A): IsEq[A] = IsEq(lhs, rhs)
