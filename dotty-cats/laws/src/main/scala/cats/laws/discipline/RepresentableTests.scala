package io.circe.cats.laws.discipline

import io.circe.cats.Representable
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.RepresentableLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.typelevel.discipline.Laws

trait RepresentableTests[F[_], R] extends Laws {
  val laws: RepresentableLaws[F, R]

  def representable[A] given Arbitrary[A], Arbitrary[F[A]], Arbitrary[R], Arbitrary[R => A], Eq[A], Eq[F[A]]: RuleSet = new DefaultRuleSet(
    name = "representable",
    parent = None,
    "index andThen tabulate = id" -> Prop.forAll(laws.indexTabulateIsId[A]),
    "tabulate andThen index = id" -> Prop.forAll(laws.tabulateIndexIsId[A])
  )
}

object RepresentableTests {
  def apply[F[_], R] given Representable.Aux[F, R]: RepresentableTests[F, R] = new RepresentableTests[F, R] {
    val laws: RepresentableLaws[F, R] = RepresentableLaws[F, R]
  }
}
