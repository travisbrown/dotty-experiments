package io.circe.cats.tests

import minitest.SimpleTestSuite
import minitest.api.SourceLocation
import minitest.laws.Checkers
import org.typelevel.discipline.Laws

class CatsSuite extends SimpleTestSuite with Checkers {
  given as SourceLocation = SourceLocation(None, None, 0)

  def checkAll(name: String, ruleSet: Laws#RuleSet): Unit =
    for ((id, prop) <- ruleSet.all.properties)
      test(name + "." + id) {
        check(prop)
      }
}

/*import io.circe.cats.kernel.Eq
import org.scalactic.anyvals.{PosInt, PosZDouble, PosZInt}
import org.scalactic.source.Position
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.scalatest.Discipline

trait TestSettings extends Configuration {

  lazy val checkConfiguration: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = PosInt.from(50).get,
      maxDiscardedFactor = PosZDouble.from(5.0).get,
      minSize = PosZInt.from(0).get,
      sizeRange = PosZInt.from(10).get,
      workers = PosInt.from(2).get
    )

  lazy val slowCheckConfiguration: PropertyCheckConfiguration =
    checkConfiguration
}

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Cats tests.
 */
trait CatsSuite
    extends AnyFunSuiteLike
    with ScalaCheckDrivenPropertyChecks
    with Discipline
    with TestSettings
    with StrictCatsEquality {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration

  implicit val pos: Position = new Position("", "", 0)

  def even(i: Int): Boolean = i % 2 == 0

  val evenPf: PartialFunction[Int, Int] = { case i if even(i) => i }
}

trait SlowCatsSuite extends CatsSuite {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    slowCheckConfiguration
}
*/