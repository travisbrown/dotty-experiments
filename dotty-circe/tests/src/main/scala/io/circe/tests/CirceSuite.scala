package io.circe.tests

import io.circe.testing.{ ArbitraryInstances, EqInstances }
import minitest.SimpleTestSuite
import minitest.api.SourceLocation
import minitest.laws.Checkers
import org.typelevel.discipline.Laws
import scala.language.implicitConversions

/**
 * An opinionated stack of traits to improve consistency and reduce boilerplate in circe tests.
 */
class CirceSuite
    extends SimpleTestSuite with Checkers
    with ArbitraryInstances
    with EqInstances
    with MissingInstances {
  given as SourceLocation = SourceLocation(None, None, 0)

  def checkLaws(name: String, ruleSet: Laws#RuleSet): Unit =
    for ((id, prop) <- ruleSet.all.properties)
      test(name + "." + id) {
        check(prop)
      }
}
