package io.circe.cats.kernel

import io.circe.cats.kernel.instances.TupleCommutativeGroupInstances
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.specialized

/**
 * An commutative group (also known as an abelian group) is a group
 * whose combine operation is commutative.
 */
trait CommutativeGroup[@specialized(Int, Long, Float, Double) A] extends Any with Group[A] with CommutativeMonoid[A]

object CommutativeGroup extends GroupFunctions[CommutativeGroup] with TupleCommutativeGroupInstances {

  /**
   * Access a given `CommutativeGroup[A]`.
   */
  def apply[A] given (A: CommutativeGroup[A]): CommutativeGroup[A] = A

  given as CommutativeGroup[Unit] = io.circe.cats.kernel.instances.UnitInstance
  given as CommutativeGroup[Byte] = io.circe.cats.kernel.instances.ByteInstance
  given as CommutativeGroup[Int] = io.circe.cats.kernel.instances.IntInstance
  given as CommutativeGroup[Short] = io.circe.cats.kernel.instances.ShortInstance
  given as CommutativeGroup[Long] = io.circe.cats.kernel.instances.LongInstance
  given as CommutativeGroup[BigInt] = io.circe.cats.kernel.instances.BigIntInstance
  given as CommutativeGroup[BigDecimal] = io.circe.cats.kernel.instances.BigDecimalInstance
  given as CommutativeGroup[Duration] = io.circe.cats.kernel.instances.DurationInstance
  given as CommutativeGroup[FiniteDuration] = io.circe.cats.kernel.instances.FiniteDurationInstance
  given as CommutativeGroup[Double] = io.circe.cats.kernel.instances.DoubleInstance
  given as CommutativeGroup[Float] = io.circe.cats.kernel.instances.FloatInstance
}
