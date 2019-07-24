package io.circe.cats.laws.discipline

import io.circe.cats.kernel.Eq
import io.circe.cats.kernel.laws.IsEq
import org.scalacheck.Prop
import org.scalacheck.util.Pretty
import scala.language.implicitConversions

export given io.circe.cats.kernel.laws.IsEqOps

implicit def catsLawsIsEqToProp[A](isEq: IsEq[A]) given Eq[A], (A => Pretty): Prop =
  io.circe.cats.kernel.laws.discipline.catsLawsIsEqToProp[A](isEq)
