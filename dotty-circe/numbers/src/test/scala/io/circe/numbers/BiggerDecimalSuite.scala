package io.circe.numbers

import io.circe.numbers.testing.{ IntegralString, JsonNumberString }
import java.math.{ BigDecimal, BigInteger }
import minitest.SimpleTestSuite
import minitest.api.SourceLocation
import minitest.laws.Checkers
import org.scalacheck.Test.Parameters
import scala.math.{ BigDecimal => SBigDecimal }
import scala.util.Try

object BiggerDecimalSuite extends SimpleTestSuite with Checkers {
  given as SourceLocation = SourceLocation(None, None, 0)

  override def checkConfig: Parameters = Parameters.default.withMinSuccessfulTests(1000).withMaxSize(10000)

  private[this] def doubleEqv(x: Double, y: Double): Boolean = java.lang.Double.compare(x, y) == 0
  private[this] def trailingZeros(i: BigInt): Int = i.toString.reverse.takeWhile(_ == '0').size
  private[this] def significantDigits(i: BigInt): Int = i.toString.size - trailingZeros(i)

  test("fromDoubleUnsafe(0) should equal fromBigDecimal(ZERO) (#348)") {
    assert(BiggerDecimal.fromDoubleUnsafe(0) == BiggerDecimal.fromBigDecimal(BigDecimal.ZERO))
  }

  test("fromDoubleUnsafe should round-trip Double values") {
    check1 { (value: Double) =>
      val d = BiggerDecimal.fromDoubleUnsafe(value)

      doubleEqv(d.toDouble, value) && d.toBigDecimal.exists { roundTripped =>
        doubleEqv(roundTripped.doubleValue, value)
      }
    }
  }

  test("fromDoubleUnsafe should round-trip negative zero") {
    val d = BiggerDecimal.fromDoubleUnsafe(-0.0)

    assert(doubleEqv(d.toDouble, -0.0))
  }

 test("signum should agree with BigInteger") {
    check1 { (value: BigInt) =>
      val d = BiggerDecimal.fromBigInteger(value.underlying)

      d.signum == value.signum
    }
  }

  test("signum should agree with BigDecimal") {
    check1 { (value: SBigDecimal) =>
      val d = BiggerDecimal.fromBigDecimal(value.underlying)

      d.signum == value.signum
    }
  }

  test("signum should agree with Long") {
    check1 { (value: Long) =>
      val d = BiggerDecimal.fromLong(value)

      d.signum == value.signum
    }
  }

  test("signum should agree with Double") {
    check1 { (value: Double) =>
      val d = BiggerDecimal.fromDoubleUnsafe(value)

      d.signum == value.signum
    }
  }

  test("fromLong should round-trip Long values") {
    check1 { (value: Long) =>
      val d = BiggerDecimal.fromLong(value)

      d.toBigDecimal.map(_.longValue) == Some(value)
    }
  }

  test("toLong should round-trip Long values") {
    check1 { (value: Long) =>
      val d = BiggerDecimal.fromLong(value)

      d.toLong == Some(value)
    }
  }

  test("toBigInteger should fail on very large values") {
    val Some(d) = BiggerDecimal.parseBiggerDecimal("1e262144")

    assert(d.toBigInteger == None)
  }

  test("not count the sign against the digit length") {
    val Some(d) = BiggerDecimal.parseBiggerDecimal("-1e262143")

    assert(d.toBigInteger == Some(new BigDecimal("-1e262143").toBigInteger))
  }

  test("toBigIntegerWithMaxDigits should fail on values whose representation is too large") {
    val Some(d) = BiggerDecimal.parseBiggerDecimal("123456789")

    assert(d.toBigIntegerWithMaxDigits(BigInteger.valueOf(8L)) == None)
  }

  test("succeed when the representation is exactly the maximum size") {
    val Some(d) = BiggerDecimal.parseBiggerDecimal("123456789")

    assert(d.toBigIntegerWithMaxDigits(BigInteger.valueOf(9L)) == d.toBigInteger)
  }

  test("fromLong and fromDoubleUnsafe should agree on Int-sized integral values") {
    check1 { (value: Int) =>
      val dl = BiggerDecimal.fromLong(value.toLong)
      val dd = BiggerDecimal.fromDoubleUnsafe(value.toDouble)

      dl == dd
    }
  }

  test("fromBigDecimal should round-trip BigDecimal values") {
    check1 { (value: SBigDecimal) =>
      val result = BiggerDecimal.fromBigDecimal(value.underlying)

      Try(new BigDecimal(value.toString)).toOption.forall { parsedValue =>
        result.toBigDecimal.exists { roundTripped =>
          roundTripped.compareTo(parsedValue) == 0
        }
      }
    }
  }

  /**
   * This is a workaround for a Scala.js bug that causes `BigDecimal` values
   * with sufficiently large exponents to be printed with negative exponents.
   *
   * The filter below will have no effect on JVM tests since the condition is
   * clearly nonsense.
   */
  private[this] def isBadJsBigDecimal(d: SBigDecimal): Boolean =
    d.abs > 1 && d.toString.contains("E-")

  test("fromBigDecimal should agree with parseBiggerDecimalUnsafe") {
    check1 { (value: SBigDecimal) =>
      isBadJsBigDecimal(value) || {
        val expected = BiggerDecimal.parseBiggerDecimalUnsafe(value.toString)

        BiggerDecimal.fromBigDecimal(value.underlying) == expected
      }
    }
  }

  test("fromBigDecimal should agree with parseBiggerDecimalUnsafe on 0.000") {
    val value = "0.000"
    val expected = BiggerDecimal.parseBiggerDecimalUnsafe(value)

    assert(BiggerDecimal.fromBigDecimal(new BigDecimal(value)) == expected)
  }

  test("fromBigDecimal should agree with parseBiggerDecimalUnsafe on multiples of ten with trailing zeros") {
    val bigDecimal = new BigDecimal("10.0")
    val fromBigDecimal = BiggerDecimal.fromBigDecimal(bigDecimal)
    val fromString = BiggerDecimal.parseBiggerDecimalUnsafe(bigDecimal.toString)

    assert(fromBigDecimal == fromString)
  }

  test("fromBigDecimal should work correctly on values whose string representations have exponents larger than Int.MaxValue") {
    val bigDecimal = new BigDecimal("-17014118346046923173168730371588410572800E+2147483647")
    val fromBigDecimal = BiggerDecimal.fromBigDecimal(bigDecimal)
    val fromString = BiggerDecimal.parseBiggerDecimalUnsafe(bigDecimal.toString)

    assert(fromBigDecimal == fromString)
  }

  test("fromBigInteger should round-trip BigInteger values" ) {
    check1 { (value: BigInt) =>
      BiggerDecimal.fromBigInteger(value.underlying).toBigInteger == Some(value.underlying)
    }
  }

  test("integralIsValidLong should agree with toLong" ) {
    check1 { (input: IntegralString) =>
      BiggerDecimal.integralIsValidLong(input.value) == Try(input.value.toLong).isSuccess
    }
  }

  test("parseBiggerDecimal should parse any BigDecimal string" ) {
    check1 { (value: SBigDecimal) =>
      val d = BiggerDecimal.parseBiggerDecimal(value.toString)

      d.nonEmpty && Try(new BigDecimal(value.toString)).toOption.forall { parsedValue =>
        d.flatMap(_.toBigDecimal).exists { roundTripped =>
          roundTripped.compareTo(parsedValue) == 0
        }
      }
    }
  }

  test("parse number strings with big exponents") {
    check3 { (integral: BigInt, fractionalDigits: BigInt, exponent: BigInt) =>
      val fractional = fractionalDigits.abs
      val s = s"$integral.${fractional}e$exponent"

      val scale = -exponent + (
        (integral == 0, fractional == 0) match {
          case (true, true) => 0
          case (_, true)    => -trailingZeros(integral)
          case (_, _)       => significantDigits(fractional)
        }
      )

      (BiggerDecimal.parseBiggerDecimal(s), Try(new BigDecimal(s)).toOption) match {
        case (Some(parsedBiggerDecimal), Some(parsedBigDecimal)) if scale.isValidInt =>
          parsedBiggerDecimal.toBigDecimal.exists(_.compareTo(parsedBigDecimal) == 0)
        case (Some(_), None) => true
        case _               => false
      }
    }
  }

  test("parseBiggerDecimal should parse JSON numbers" ) {
    check1 { (jns: JsonNumberString) =>
      BiggerDecimal.parseBiggerDecimal(jns.value).nonEmpty
    }
  }

  test("parseBiggerDecimal should parse integral JSON numbers" ) {
    check1 { (is: IntegralString) =>
      BiggerDecimal.parseBiggerDecimal(is.value) == Some(BiggerDecimal.fromBigInteger(new BigInteger(is.value)))
    }
  }

  test("parseBiggerDecimal should fail on bad input") {
    val badNumbers = List("", "x", "01", "1x", "1ex", "1.0x", "1.x", "1e-x", "1e-0x", "1.", "1e", "1e-", "-")

    badNumbers.foreach { input =>
      BiggerDecimal.parseBiggerDecimal(input) == None
    }
  }
}
