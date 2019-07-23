package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.{Eq, Hash}
import io.circe.cats.kernel.laws.HashLaws
import org.scalacheck.{Arbitrary, Prop}
import scala.util.hashing.Hashing

trait HashTests[A] extends EqTests[A] {
  def laws: HashLaws[A]

  def hash given Arbitrary[A], Arbitrary[A => A], Eq[A], Hashing[A]: RuleSet =
    new DefaultRuleSet(
      "hash",
      Some(eqv),
      "hash compatibility" -> Prop.forAll(laws.hashCompatibility),
      "same as universal hash" -> Prop.forAll(laws.sameAsUniversalHash),
      "same as scala hashing" -> Prop.forAll((x: A, y: A) => laws.sameAsScalaHashing(x, y, the[Hashing[A]]))
    )

}

object HashTests {
  def apply[A] given Hash[A]: HashTests[A] =
    new HashTests[A] with EqTests[A] { def laws: HashLaws[A] = HashLaws[A] }
}
