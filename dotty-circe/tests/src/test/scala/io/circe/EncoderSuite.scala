package io.circe

import cats.data.Chain
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.ContravariantTests
import given cats.syntax.eq._
import given io.circe.syntax._
import io.circe.tests.CirceSuite
import org.scalacheck.Arbitrary
import scala.collection.SortedMap

object EncoderSuite extends CirceSuite {
  checkLaws("Encoder[Int]", ContravariantTests[Encoder].contravariant[Int, Int, Int])
  checkLaws("Encoder.AsArray[Int]", ContravariantTests[Encoder.AsArray].contravariant[Int, Int, Int])
  checkLaws("Encoder.AsObject[Int]", ContravariantTests[Encoder.AsObject].contravariant[Int, Int, Int])

  test("mapJson should transform encoded output") {
    check3 { (m: Map[String, Int], k: String, v: Int) =>
      val newEncoder = Encoder[Map[String, Int]].mapJson(
        _.withObject(obj => Json.fromJsonObject(obj.add(k, v.asJson)))
      )

      Decoder[Map[String, Int]].apply(newEncoder(m).hcursor) === Right(m.updated(k, v))
    }
  }

  test("Encoder.AsObject#mapJsonObject should transform encoded output") {
    check3 { (m: Map[String, Int], k: String, v: Int) =>
      val newEncoder = Encoder.AsObject[Map[String, Int]].mapJsonObject(_.add(k, v.asJson))

      Decoder[Map[String, Int]].apply(newEncoder(m).hcursor) === Right(m.updated(k, v))
    }
  }

  /*test("Encoder[Enumeration] should write Scala Enumerations") {
    object WeekDay extends Enumeration {
      type WeekDay = Value
      val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
    }

    implicit val encoder = Encoder.encodeEnumeration(WeekDay)
    val json = WeekDay.Fri.asJson
    val decoder = Decoder.decodeEnumeration(WeekDay)
    assert(decoder.apply(json.hcursor) === Right(WeekDay.Fri))
  }*/

  test("encodeSet should match sequence encoders") {
    check1 { (xs: Set[Int]) =>
      Encoder.encodeSet[Int].apply(xs) === Encoder[Seq[Int]].apply(xs.toSeq)
    }
  }

  test("encodeList should match sequence encoders") {
    check1 { (xs: List[Int]) =>
      Encoder.encodeList[Int].apply(xs) === Encoder[Seq[Int]].apply(xs)
    }
  }

  case class MyString(value: String)

  object MyString {
    implicit val myStringOrdering: Ordering[MyString] = Ordering.by[MyString, String](_.value).reverse
    implicit val myStringKeyEncoder: KeyEncoder[MyString] = KeyEncoder.instance(_.value)
    implicit val myStringArbitrary: Arbitrary[MyString] = Arbitrary(
      Arbitrary.arbitrary[String].map(MyString(_))
    )
  }

  test("encodeMap should preserve insertion order") {
    check1 { (m: SortedMap[MyString, String]) =>
      val Some(asJsonObject) = m.asJson.asObject
      val expected = m.toList.map {
        case (k, v) => MyString.myStringKeyEncoder(k) -> Json.fromString(v)
      }

      asJsonObject.toList === expected
    }
  }

  test("encodeVector should match sequence encoders") {
    check1 { (xs: Vector[Int]) =>
      Encoder.encodeVector[Int].apply(xs) === Encoder[Seq[Int]].apply(xs)
    }
  }

  test("encodeChain should match sequence encoders") {
    check1 { (xs: Chain[Int]) =>
      Encoder.encodeChain[Int].apply(xs) === Encoder[Seq[Int]].apply(xs.toList)
    }
  }

  test("encodeFloat should match string representation") {
    check1 { x: Float =>
      // All Float values should be encoded in a way that match the original value.
      Encoder[Float].apply(x).toString.toFloat === x
    }
  }

  test("encodeFloat should match Float.toString when not represented with scientific notation") {
    check1 { x: Float =>

    // For floats which are NOT represented with scientific notation,
    // the JSON representaton should match Float.toString
    // This should catch cases where 1.2f would previously be encoded
    // as 1.2000000476837158 due to the use of .toDouble
      !x.toString.toLowerCase.contains('e') || Encoder[Float].apply(x).toString === x.toString
    }
  }
}
