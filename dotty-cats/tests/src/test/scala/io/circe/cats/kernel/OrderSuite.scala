package io.circe.cats.kernel

import io.circe.cats.{Contravariant, ContravariantMonoidal, Invariant}
import io.circe.cats.kernel.laws.discipline.{OrderTests, SerializableTests}
import io.circe.cats.laws.discipline.{ContravariantMonoidalTests, MiniInt}
import io.circe.cats.laws.discipline.arbitrary._
import io.circe.cats.laws.discipline.eq._
import given io.circe.cats.syntax.eq._
import given io.circe.cats.syntax.order._
import given io.circe.cats.syntax.partialOrder._
import io.circe.cats.tests.CatsSuite
import io.circe.cats.tests.Helpers.Ord

object OrderSuite extends CatsSuite {
  {
    Invariant[Order]
    Contravariant[Order]
    ContravariantMonoidal[Order]
  }

  checkAll("Int", OrderTests[Int].order)
  checkAll("Double", OrderTests[Double].order)
  checkAll("Float", OrderTests[Float].order)
  checkAll("Long", OrderTests[Long].order)

  checkAll("Order", ContravariantMonoidalTests[Order].contravariantMonoidal[MiniInt, Boolean, Boolean])
  checkAll("ContravariantMonoidal[Order]", SerializableTests.serializable(ContravariantMonoidal[Order]))

  test("order ops syntax") {
    check2 { (i: Ord, j: Ord) =>
      (i.compare(j)) === Order.compare(i, j) &&
      (i.min(j)) === Order.min(i, j) &&
      (i.max(j)) === Order.max(i, j) &&
      (i.comparison(j)) === Order.comparison(i, j) &&

      // These operators currently don't work in Dotty.
      // partial order syntax should also work when an Order instance exists
      //(i > j) === PartialOrder.gt(i, j) &&
      //(i >= j) === PartialOrder.gteqv(i, j) &&
      //(i < j) === PartialOrder.lt(i, j) &&
      //(i <= j) === PartialOrder.lteqv(i, j) &&

      (i.partialCompare(j)) === PartialOrder.partialCompare(i, j) &&
      (i.tryCompare(j)) === PartialOrder.tryCompare(i, j) &&
      (i.pmin(j)) === PartialOrder.pmin(i, j) &&
      (i.pmax(j)) === PartialOrder.pmax(i, j)
    }
  }

  def summonInstance(): Unit = {
    Invariant[Order]
    Contravariant[Order]
    ContravariantMonoidal[Order]
    ()
  }

  // ambiguity test:
  // the Ordering instance from the Order instance should be trumped
  // by the one provided in the Ordering companion object
  {
    Ordering[String]
    class C
    implicit val ording: Ordering[C] = new Ordering[C] {
      def compare(x: C, y: C) = 0
    }
    implicit val ord: Order[C] = Order.allEqual
    Ordering[C]
  }
}
