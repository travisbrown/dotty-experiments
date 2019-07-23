package io.circe.cats.kernel

import io.circe.cats.kernel.instances.TupleBandInstances
import scala.specialized

/**
 * Bands are semigroups whose operation
 * (i.e. combine) is also idempotent.
 */
trait Band[@specialized(Int, Long, Float, Double) A] extends Any with Semigroup[A]

object Band extends SemigroupFunctions[Band] with TupleBandInstances {

  /**
   * Access a given `Band[A]`.
   */
  def apply[@specialized(Int, Long, Float, Double) A] given (A: Band[A]): Band[A] = A

  /**
   * Create a `Band` instance from the given function.
   */
  def instance[A](cmb: (A, A) => A): Band[A] = new Band[A] {
    override def combine(x: A, y: A): A = cmb(x, y)
  }

  given [A] as Band[() => A] given Band[A] = io.circe.cats.kernel.instances.Function0Band[A]
}
