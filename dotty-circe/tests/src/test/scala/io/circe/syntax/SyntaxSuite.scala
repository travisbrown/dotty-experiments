package io.circe.syntax_test

import io.circe.{ Encoder, Json, KeyEncoder }
import given io.circe.cats.syntax.eq._
import given io.circe.syntax._
import io.circe.tests.CirceSuite

object SyntaxSuite extends CirceSuite {
  test("asJson should be available and work appropriately") {
    check1 { (s: String) =>
      // This doesn't compile in-line (TODO: minimize).
      val result = s.asJson
      result === Json.fromString(s)
    }
  }

  test("asJsonObject should be available and work appropriately") {
    check1 { (m: Map[String, Int]) =>
      // This doesn't compile in-line (TODO: minimize).
      val result = m.asJsonObject
      result === Encoder[Map[String, Int]].apply(m).asObject.get
    }
  }

  test(":= should be available and work with String keys") {
   check5 { (key: String, m: Map[String, Int], aNumber: Int, aString: String, aBoolean: Boolean) =>
      (key := m) === (key, m.asJson) &&
      (key := aNumber) === (key, aNumber.asJson) &&
      (key := aString) === (key, aString.asJson) &&
      (key := aBoolean) === (key, aBoolean.asJson)
    }
  }

  test(":= should be available and work with non-String keys that have a KeyEncoder instance") {
    case class CustomKey(componentOne: String, componentTwo: Int)
    implicit val keyEncoder: KeyEncoder[CustomKey] =
      KeyEncoder[String].contramap(k => s"${k.componentOne}_${k.componentTwo}")

    check4 {
      (
        m: Map[String, Int],
        aNumber: Int,
        aString: String,
        aBoolean: Boolean
      ) =>
        {
          val key = CustomKey("keyComponentOne", 2)
          val keyStringRepresentation = "keyComponentOne_2"
          (key := m) === (keyStringRepresentation, m.asJson) &&
          (key := aNumber) === (keyStringRepresentation, aNumber.asJson) &&
          (key := aString) === (keyStringRepresentation, aString.asJson) &&
          (key := aBoolean) === (keyStringRepresentation, aBoolean.asJson)
        }
    }
  }
}
