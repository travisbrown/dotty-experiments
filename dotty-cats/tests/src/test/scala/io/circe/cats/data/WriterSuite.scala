package io.circe.cats.data

import given io.circe.cats.syntax.applicative._
import given io.circe.cats.syntax.eq._
import given io.circe.cats.syntax.writer._
import io.circe.cats.tests.CatsSuite

class WriterSuite extends CatsSuite {
  test("pure syntax creates a writer with an empty log") {
    check1 { (result: String) =>
      type Logged[A] = Writer[List[Int], A]
       Writer(List.empty[Int], result) === result.pure[Logged]
    }
  }

  test("tell syntax creates a writer with a unit result") {
    check1 { (log: List[Int]) =>
      log.tell === Writer(log, ())
    }
  }

  test("writer syntax creates a writer with the specified result and log") {
    check2 { (result: String, log: List[Int]) =>
      result.writer(log) === Writer(log, result)
    }
  }
}
