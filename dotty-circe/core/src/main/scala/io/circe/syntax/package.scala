package io.circe

/**
 * This package provides syntax via extension methods.
 */
package object syntax {
  given [A] {
    def (a: A) asJson given (A: Encoder[A]): Json = A(a)
    def (a: A) asJsonObject given (A: Encoder.AsObject[A]): JsonObject =
      A.encodeObject(a)
    def (a: A) := [V](v: V) given (A: KeyEncoder[A], V: Encoder[V]) = (A(a), V(v))
  }
}
