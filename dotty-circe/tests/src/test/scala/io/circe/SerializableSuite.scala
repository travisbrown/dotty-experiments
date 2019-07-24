package io.circe

import cats.kernel.laws.discipline.SerializableTests
import cats.kernel.laws.SerializableLaws
import io.circe.tests.CirceSuite

object SerializableSuite extends CirceSuite {
  test("Json should be serializable") {
    check1 { (j: Json) =>
      SerializableLaws.serializable(j)
      true
    }
  }

  test("HCursor should be serializable") {
    check1 { (j: Json) =>
      SerializableLaws.serializable(j.hcursor)
      true
    }
  }

  checkLaws("Decoder[Int]", SerializableTests.serializable(Decoder[Int]))
  checkLaws("Encoder[Int]", SerializableTests.serializable(Encoder[Int]))

  checkLaws(
    "Encoder.AsArray[List[String]]",
    SerializableTests.serializable(Encoder.AsArray[List[String]])
  )

  checkLaws(
    "Encoder.AsObject[Map[String, Int]]",
    SerializableTests.serializable(Encoder.AsObject[Map[String, Int]])
  )

  checkLaws("Parser", SerializableTests.serializable(jawn.`package`))
  checkLaws("Printer", SerializableTests.serializable(Printer.noSpaces))
}
