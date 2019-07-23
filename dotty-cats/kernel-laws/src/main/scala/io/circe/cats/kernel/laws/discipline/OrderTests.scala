package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.{Eq, Order}
import io.circe.cats.kernel.instances.TupleOrderInstances
import io.circe.cats.kernel.laws.OrderLaws
import org.scalacheck.{Arbitrary, Prop}

trait OrderTests[A] extends PartialOrderTests[A] {
  def laws: OrderLaws[A]

  def order given Arbitrary[A], Arbitrary[A => A], Eq[Option[A]], Eq[A]: RuleSet =
    new DefaultRuleSet(
      "order",
      Some(partialOrder),
      "totality" -> Prop.forAll(laws.totality),
      "compare" -> Prop.forAll(laws.compare),
      "max" -> Prop.forAll(laws.max),
      "min" -> Prop.forAll(laws.min)
    )
}

object OrderTests {
  def apply[A] given Order[A]: OrderTests[A] =
    new OrderTests[A] { def laws: OrderLaws[A] = OrderLaws[A] }
}
