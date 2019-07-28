package io.circe.cats.kernel

import scala.util.hashing.Hashing

/**
 * A type class used to represent a hashing scheme for objects of a given type.
 * For any two instances `x` and `y` that are considered equivalent under the
 * equivalence relation defined by this object, `hash(x)` should equal `hash(y)`.
 * @author Tongfei Chen
 */
trait Hash[@specialized A] extends Any with Eq[A] with Serializable { self =>

  /**
   * Returns the hash code of the given object under this hashing scheme.
   */
  def hash(x: A): Int

  // `Hash#toHashing` deliberately not implemented since `scala.util.hashing.Hashing` is only
  // compatible with universal equality.
}

object Hash {

  /** Fetch a `Hash` instance given the specific type. */
  def apply[A] given (A: Hash[A]): Hash[A] = A

  def by[@specialized A, @specialized B](f: A => B) given (B: Hash[B]): Hash[A] =
    new Hash[A] {
      def hash(x: A) = B.hash(f(x))
      def eqv(x: A, y: A) = B.eqv(f(x), f(y))
    }

  def fromHashing[A](ev: Hashing[A]): Hash[A] =
    new Hash[A] {
      def hash(x: A) = ev.hash(x)
      def eqv(x: A, y: A) = x == y // universal equality
    }

  /**
   * Constructs a `Hash` instance by using the universal `hashCode` function and the universal equality relation.
   */
  def fromUniversalHashCode[A]: Hash[A] =
    new Hash[A] {
      def hash(x: A) = x.hashCode()
      def eqv(x: A, y: A) = x == y
    }

  def hash[@specialized A](x: A) given (A: Hash[A]): Int = A.hash(x)

  given [A] as Hashing[A] given Hash[A] {
    def hash(x: A): Int = Hash.hash(x)
  }
}
