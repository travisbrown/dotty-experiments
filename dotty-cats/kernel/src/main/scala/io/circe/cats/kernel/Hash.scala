package io.circe.cats.kernel

import io.circe.cats.kernel.instances.TupleHashInstances
import java.util.UUID
import scala.collection.immutable.{BitSet, Queue, SortedMap, SortedSet}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.specialized
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

object Hash extends TupleHashInstances {

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

  given as Hash[Unit] = io.circe.cats.kernel.instances.UnitInstance
  given as Hash[Boolean] = io.circe.cats.kernel.instances.BooleanInstance
  given as Hash[Byte] = io.circe.cats.kernel.instances.ByteInstance
  given as Hash[Int] = io.circe.cats.kernel.instances.IntInstance
  given as Hash[Short] = io.circe.cats.kernel.instances.ShortInstance
  given as Hash[Long] = io.circe.cats.kernel.instances.LongInstance
  given as Hash[BigInt] = io.circe.cats.kernel.instances.BigIntInstance
  given as Hash[BigDecimal] = io.circe.cats.kernel.instances.BigDecimalInstance
  given as Hash[Duration] = io.circe.cats.kernel.instances.DurationInstance
  given as Hash[FiniteDuration] = io.circe.cats.kernel.instances.FiniteDurationInstance
  given as Hash[Char] = io.circe.cats.kernel.instances.CharInstance
  given as Hash[Symbol] = io.circe.cats.kernel.instances.SymbolInstance
  given as Hash[String] = io.circe.cats.kernel.instances.StringInstance
  given as Hash[UUID] = io.circe.cats.kernel.instances.UUIDInstance
  given as Hash[BitSet] = io.circe.cats.kernel.instances.BitSetInstance
  given as Hash[Double] = io.circe.cats.kernel.instances.DoubleInstance
  given as Hash[Float] = io.circe.cats.kernel.instances.FloatInstance

  given [A] as Hash[Set[A]] = io.circe.cats.kernel.instances.SetInstance[A]
  given [A] as Hash[SortedSet[A]] given Hash[A] = io.circe.cats.kernel.instances.SortedSetHash[A]
  given [A] as Hash[Option[A]] given Hash[A] = io.circe.cats.kernel.instances.OptionHash[A]
  given [A] as Hash[List[A]] given Hash[A] = io.circe.cats.kernel.instances.ListHash[A]
  given [A] as Hash[Vector[A]] given Hash[A] = io.circe.cats.kernel.instances.VectorHash[A]
  given [A] as Hash[Stream[A]] given Hash[A] = io.circe.cats.kernel.instances.StreamHash[A]
  given [A] as Hash[Queue[A]] given Hash[A] = io.circe.cats.kernel.instances.QueueHash[A]
  given [K, V] as Hash[Map[K, V]] given Hash[V] = io.circe.cats.kernel.instances.MapHash[K, V]
  given [K, V] as Hash[SortedMap[K, V]] given Hash[K], Order[K], Hash[V] = io.circe.cats.kernel.instances.SortedMapHash[K, V]
  given [A, B] as Hash[Either[A, B]] given Hash[A], Hash[B] = io.circe.cats.kernel.instances.EitherHash[A, B]

  given [A] as Hashing[A] given Hash[A] {
    def hash(x: A): Int = Hash.hash(x)
  }
}
