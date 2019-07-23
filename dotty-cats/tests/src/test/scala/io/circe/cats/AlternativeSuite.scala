package io.circe.cats

import io.circe.cats.tests.CatsSuite

object AlternativeSuite extends CatsSuite {
  test("unite") {
    check1 { (list: List[Option[String]]) =>
      val expected = list.collect { case Some(s) => s }

      Alternative[List].unite(list) == expected
    }
  }

  test("separate") {
    check1 { (list: List[Either[Int, String]]) =>
      val ints = list.collect { case Left(i)     => i }
      val strings = list.collect { case Right(s) => s }
      val expected = (ints, strings)

      Alternative[List].separate(list) == expected
    }
  }

  test("guard") {
    assert(Alternative[Option].guard(true).isDefined)
    assert(Alternative[Option].guard(false).isEmpty)
  }
}
