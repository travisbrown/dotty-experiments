package io.circe.cats.laws.discipline

import io.circe.cats.Eval
import io.circe.cats.laws.discipline.MonadTests
import io.circe.cats.laws.discipline.arbitrary._
import io.circe.cats.laws.discipline.eq._
import minitest.SimpleTestSuite
import minitest.api.SourceLocation
import minitest.laws.Checkers
import org.typelevel.discipline.Laws

class DisciplineSuite extends SimpleTestSuite with Checkers {
  given as SourceLocation = SourceLocation(None, None, 0)

  def checkAll(name: String, ruleSet: Laws#RuleSet): Unit =
    for ((id, prop) <- ruleSet.all.properties)
      test(name + "." + id) {
        check(prop)
      }
}

object MonadTestsSuite extends DisciplineSuite {
  // We don't use `stackUnsafeMonad` in our laws checking for instances in Cats,
  // so we confirm here that the laws pass for `Eval` (the monad instance for
  // which is actually stack safe, like all other monad instances in Cats.)
  checkAll("Eval[Int]", MonadTests[Eval].stackUnsafeMonad[Int, Int, Int])
}
