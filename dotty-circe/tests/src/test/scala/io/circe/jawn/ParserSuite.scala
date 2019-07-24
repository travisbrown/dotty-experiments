package io.circe.jawn

import io.circe.Json
import io.circe.testing.ParserTests
import io.circe.tests.CirceSuite

object ParserSuite extends CirceSuite {
  checkLaws("Parser", ParserTests(`package`).fromString)

  test("parse and decode(Accumulating) should fail on invalid input") {
  	check1 { (s: String) =>
      io.circe.jawn.parse(s"Not JSON $s").isLeft && 
      io.circe.jawn.decode[Json](s"Not JSON $s").isLeft &&
      io.circe.jawn.decodeAccumulating[Json](s"Not JSON $s").isInvalid
    }
  }
}
