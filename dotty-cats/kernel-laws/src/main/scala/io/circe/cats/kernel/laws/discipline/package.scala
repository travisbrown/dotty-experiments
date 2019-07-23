package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.Eq
import io.circe.cats.kernel.laws.IsEq
import org.scalacheck.Prop
import org.scalacheck.util.Pretty
import scala.language.implicitConversions

implicit def catsLawsIsEqToProp[A](isEq: IsEq[A]) given Eq[A], (A => Pretty): Prop =
  isEq match {
    case IsEq(x, y) =>
      if (the[Eq[A]].eqv(x, y)) Prop.proved
      else
        Prop.falsified :| {
          val exp = Pretty.pretty[A](y, Pretty.Params(0))
          val act = Pretty.pretty[A](x, Pretty.Params(0))
          s"Expected: $exp\n" + s"Received: $act"
        }
  }
