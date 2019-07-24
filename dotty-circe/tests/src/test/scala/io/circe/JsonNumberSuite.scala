package io.circe

import given cats.syntax.eq._
import io.circe.numbers.testing.JsonNumberString
import io.circe.tests.CirceSuite

object JsonNumberSuite extends CirceSuite {
  test("fromString should parse valid JSON numbers") {
    check1 { (jsn: JsonNumberString) =>
      JsonNumber.fromString(jsn.value).nonEmpty
    }
  }

  test("fromString should match Json.fromDouble") {
    check1 { (d: Double) =>
      val expected = Json.fromDouble(d).flatMap(_.asNumber)

      JsonNumber.fromString(d.toString) === expected
    }
  }

  test("fromString should match Json.fromFloat") {
    check1 { (f: Float) =>
      val expected = Json.fromFloat(f).flatMap(_.asNumber)

      JsonNumber.fromString(f.toString) === expected
    }
  }

  test("fromString should match Json.fromFloat for Floats that don't have the same toString when Double-ed") {
    val value = -4.9913575e19f

    assert(Json.fromFloat(value).flatMap(_.asNumber) === JsonNumber.fromString(value.toString))
  }

  test("fromString should round-trip Byte") {
    check1 { (b: Byte) =>
      JsonNumber.fromString(b.toString).flatMap(_.toByte) === Some(b)
    }
  }

  test("fromString should round-trip Short") {
    check1 { (s: Short) =>
      JsonNumber.fromString(s.toString).flatMap(_.toShort) === Some(s)
    }
  }

  test("fromString should round-trip Int") {
    check1 { (i: Int) =>
      JsonNumber.fromString(i.toString).flatMap(_.toInt) === Some(i)
    }
  }

  test("fromString should round-trip Long") {
    check1 { (l: Long) =>
      JsonNumber.fromString(l.toString).flatMap(_.toLong) === Some(l)
    }
  }

  test("toByte should fail on out-of-range values") {
    check1 { (l: Long) =>
      val invalid = l > Byte.MaxValue || l < Byte.MinValue

      JsonNumber.fromString(l.toString).flatMap(_.toByte).isEmpty === invalid
    }
  }

  test("toShort should fail on out-of-range values") {
    check1 { (l: Long) =>
      val invalid = l > Short.MaxValue || l < Short.MinValue

      JsonNumber.fromString(l.toString).flatMap(_.toShort).isEmpty === invalid
    }
  }

  test("toInt should fail on out-of-range values") {
    check1 { (l: Long) =>
      val invalid = l > Int.MaxValue || l < Int.MinValue

      JsonNumber.fromString(l.toString).flatMap(_.toInt).isEmpty === invalid
    }
  }

  test("JsonFloat.toLong should return None if outside of Long bounds") {
    check1 { (f: Float) =>
      if (f < Long.MinValue || f > Long.MaxValue) {
        JsonFloat(f).toLong === None
      } else true
    }
  }

  test("JsonFloat.toBigInt should return None if it loses precision") {
    check1 { (f: Float) =>
      val j = JsonFloat(f)
      val expected = j.toBiggerDecimal match {
        case d if d.isWhole => Some(BigDecimal(f.toString).toBigInt)
        case _              => None
      }
      j.toBigInt === expected
    }
  }

  val positiveZeros: List[JsonNumber] = List(
    JsonNumber.fromIntegralStringUnsafe("0"),
    JsonNumber.fromDecimalStringUnsafe("0.0"),
    Json.fromDouble(0.0).flatMap(_.asNumber).get,
    Json.fromFloat(0.0f).flatMap(_.asNumber).get,
    Json.fromLong(0).asNumber.get,
    Json.fromBigInt(BigInt(0)).asNumber.get,
    Json.fromBigDecimal(BigDecimal(0)).asNumber.get
  )

  val negativeZeros: List[JsonNumber] = List(
    JsonNumber.fromIntegralStringUnsafe("-0"),
    JsonNumber.fromDecimalStringUnsafe("-0.0"),
    Json.fromDouble(-0.0).flatMap(_.asNumber).get,
    Json.fromFloat(-0.0f).flatMap(_.asNumber).get
  )

  test("Eq[JsonNumber] should distinguish negative and positive zeros") {
    positiveZeros.foreach { pz =>
      negativeZeros.foreach { nz =>
        assert(!(pz === nz))
      }
    }
  }

  test("Eq[JsonNumber] should not distinguish any positive zeros") {
    positiveZeros.foreach { pz1 =>
      positiveZeros.foreach { pz2 =>
        assert(pz1 === pz2)
      }
    }
  }

  test("Eq[JsonNumber] should not distinguish any negative zeros") {
    negativeZeros.foreach { nz1 =>
      negativeZeros.foreach { nz2 =>
        assert(nz1 === nz2)
      }
    }
  }

  test("Eq[JsonNumber] should compare Float and Long") {
    check2 { (f: Float, l: Long) =>
      runCompareTest(JsonFloat, f, JsonLong, l)
    }
  }

  test("Eq[JsonNumber] should compare Float and Double") {
    check2 { (f: Float, d: Double) =>
      runCompareTest(JsonFloat, f, JsonDouble, d)
    }
  }

  test("Eq[JsonNumber] should compare Float and Float") {
    check2 { (f1: Float, f2: Float) =>
      runCompareTest(JsonFloat, f1, JsonFloat, f2)
    }
  }

  private def runCompareTest[A, B](f1: A => JsonNumber, v1: A, f2: B => JsonNumber, v2: B): Boolean = {
    val n1 = f1(v1)
    val n2 = f2(v2)
    val expected = v1 == v2
    (n1 === n2) === expected && (n2 === n1) === expected
  }

  test("fromDouble should fail on Double.NaN") {
    assert(Json.fromDouble(Double.NaN) === None)
  }

  test("fromDouble should fail on Double.PositiveInfinity") {
    assert(Json.fromDouble(Double.PositiveInfinity) === None)
  }

  test("fromDouble should fail on Double.NegativeInfinity") {
    assert(Json.fromDouble(Double.NegativeInfinity) === None)
  }

  test("fromFloat should fail on Float.Nan") {
    assert(Json.fromFloat(Float.NaN) === None)
  }

  test("fromFloat should should fail on Float.PositiveInfinity") {
    assert(Json.fromFloat(Float.PositiveInfinity) === None)
  }

  test("fromFloat should should fail on Float.NegativeInfinity") {
    assert(Json.fromFloat(Float.NegativeInfinity) === None)
  }
}
