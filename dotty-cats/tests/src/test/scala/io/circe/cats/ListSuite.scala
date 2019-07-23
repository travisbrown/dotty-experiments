package io.circe.cats

import io.circe.cats.data.{NonEmptyList}
import io.circe.cats.kernel.laws.discipline.SerializableTests
import io.circe.cats.laws.discipline.{
  AlternativeTests,
  CoflatMapTests,
  CommutativeApplyTests,
  MonadTests,
  SemigroupalTests,
  TraverseFilterTests,
  TraverseTests
}
import io.circe.cats.laws.discipline.arbitrary._
import given io.circe.cats.syntax.eq._
import given io.circe.cats.syntax.list._
import given io.circe.cats.syntax.show._
import io.circe.cats.tests.CatsSuite
import org.scalacheck.Test.Parameters

object ListSuite extends CatsSuite {
  override def checkConfig: Parameters = Parameters.default.withMaxSize(15)

  checkAll("List[Int]", SemigroupalTests[List].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[List]", SerializableTests.serializable(Semigroupal[List]))

  checkAll("List[Int]", CoflatMapTests[List].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[List]", SerializableTests.serializable(CoflatMap[List]))

  checkAll("List[Int]", AlternativeTests[List].alternative[Int, Int, Int])
  checkAll("Alternative[List]", SerializableTests.serializable(Alternative[List]))

  checkAll("List[Int] with Option", TraverseTests[List].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Traverse[List]", SerializableTests.serializable(Traverse[List]))

  checkAll("List[Int]", MonadTests[List].monad[Int, Int, Int])
  checkAll("Monad[List]", SerializableTests.serializable(Monad[List]))

  checkAll("List[Int]", TraverseFilterTests[List].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[List]", SerializableTests.serializable(TraverseFilter[List]))

  //checkAll("ZipList[Int]", CommutativeApplyTests[ZipList].commutativeApply[Int, Int, Int])

  test("nel => list => nel returns original nel")(
    check1 { fa: NonEmptyList[Int] =>
      fa.toList.toNel === Some(fa)
    }
  )

  test("toNel on empty list returns None") {
    assert(List.empty[Int].toNel === None)
  }

  test("groupByNel should be consistent with groupBy")(
    check2 { (fa: List[Int], f: Int => Int) =>
      fa.groupByNel(f).map { case (k, v) => (k, v.toList) } === fa.groupBy(f)
    }
  )

  test("show") {
    assert("List(1, 2, 3)" === List(1, 2, 3).show)
    assert("List()" === (Nil: List[Int]).show)
    check1 { l: List[String] =>
      l.toString === l.show
    }
  }
}
