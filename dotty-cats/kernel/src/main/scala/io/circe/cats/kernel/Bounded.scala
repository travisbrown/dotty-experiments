package io.circe.cats.kernel

import java.util.UUID
import scala.collection.immutable.BitSet
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
 * A type class used to name the lower limit of a type.
 */
trait LowerBounded[@specialized A] {
  def partialOrder: PartialOrder[A]

  /**
   * Returns the lower limit of a type.
   */
  def minBound: A
}

object LowerBounded {
  def apply[A] given (A: LowerBounded[A]): LowerBounded[A] = A
  def minBound[@specialized A] given (A: LowerBounded[A]): A = A.minBound

  given as LowerBounded[Unit] = io.circe.cats.kernel.instances.UnitInstance
  given as LowerBounded[Boolean] = io.circe.cats.kernel.instances.BooleanInstance
  given as LowerBounded[Byte] = io.circe.cats.kernel.instances.ByteInstance
  given as LowerBounded[Int] = io.circe.cats.kernel.instances.IntInstance
  given as LowerBounded[Short] = io.circe.cats.kernel.instances.ShortInstance
  given as LowerBounded[Long] = io.circe.cats.kernel.instances.LongInstance
  given as LowerBounded[Duration] = io.circe.cats.kernel.instances.DurationInstance
  given as LowerBounded[FiniteDuration] = io.circe.cats.kernel.instances.FiniteDurationInstance
  given as LowerBounded[Char] = io.circe.cats.kernel.instances.CharInstance
  given as LowerBounded[String] = io.circe.cats.kernel.instances.StringInstance
  given as LowerBounded[Symbol] = io.circe.cats.kernel.instances.SymbolInstance
  given as LowerBounded[UUID] = io.circe.cats.kernel.instances.UUIDInstance
}

/**
 * A type class used to name the upper limit of a type.
 */
trait UpperBounded[@specialized A] {
  def partialOrder: PartialOrder[A]

  /**
   * Returns the upper limit of a type.
   */
  def maxBound: A
}

object UpperBounded {
  def apply[A] given (A: UpperBounded[A]): UpperBounded[A] = A
  def maxBound[@specialized A] given (A: UpperBounded[A]): A = A.maxBound

  given as UpperBounded[Unit] = io.circe.cats.kernel.instances.UnitInstance
  given as UpperBounded[Boolean] = io.circe.cats.kernel.instances.BooleanInstance
  given as UpperBounded[Byte] = io.circe.cats.kernel.instances.ByteInstance
  given as UpperBounded[Int] = io.circe.cats.kernel.instances.IntInstance
  given as UpperBounded[Short] = io.circe.cats.kernel.instances.ShortInstance
  given as UpperBounded[Char] = io.circe.cats.kernel.instances.CharInstance
  given as UpperBounded[Long] = io.circe.cats.kernel.instances.LongInstance
  given as UpperBounded[Duration] = io.circe.cats.kernel.instances.DurationInstance
  given as UpperBounded[FiniteDuration] = io.circe.cats.kernel.instances.FiniteDurationInstance
  given as UpperBounded[UUID] = io.circe.cats.kernel.instances.UUIDInstance
}
