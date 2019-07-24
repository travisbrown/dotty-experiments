package io.circe.rs

import io.circe.Json
import io.circe.cats.laws.discipline.TraverseTests
import given io.circe.cats.syntax.eq._
import io.circe.rs.JsonF.{ foldJson, unfoldJson }
import io.circe.tests.CirceSuite

object JsonFSuite extends CirceSuite {
  checkLaws("Traverse[JsonF]", TraverseTests[JsonF].traverse[Int, Int, Int, Set[Int], Option, Option])

  test("fold then unfold should be identity") {
  	check1 { jsonF: JsonF[Json] =>
      unfoldJson(foldJson(jsonF)) === jsonF
    }
  }

  test("unfold then fold should be identity") {
    check1 { json: Json =>
      foldJson(unfoldJson(json)) === json
    }
  }
}
