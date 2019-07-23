package io.circe.cats.kernel.laws

import io.circe.cats.kernel.Hash
import scala.util.hashing._

trait HashLaws[A] given (A: Hash[A]) extends EqLaws[A] {
  def hashCompatibility(x: A, y: A): IsEq[Boolean] =
    (!A.eqv(x, y) || (Hash.hash(x) == Hash.hash(y))) <-> true

  def sameAsUniversalHash(x: A, y: A): IsEq[Boolean] =
    ((A.hash(x) == x.hashCode) && (Hash.fromUniversalHashCode[A].hash(x) == x.hashCode()) &&
      (A.eqv(x, y) == Hash.fromUniversalHashCode[A].eqv(x, y))) <-> true

  def sameAsScalaHashing(x: A, y: A, scalaHashing: Hashing[A]): IsEq[Boolean] =
    ((A.hash(x) == Hash.fromHashing(scalaHashing).hash(x)) &&
      (A.eqv(x, y) == Hash.fromHashing(scalaHashing).eqv(x, y))) <-> true

}

object HashLaws {
  def apply[A] given Hash[A]: HashLaws[A] =
    new HashLaws[A] with EqLaws[A]
}
