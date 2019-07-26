package io.circe.literal

import io.circe.{Encoder, Json, KeyEncoder}
import scala.language.implicitConversions

opaque type AsJson = Json

object AsJson {
  implicit def toAsJson[A](a: A) given (A: Encoder[A]): AsJson = A(a)

  given AsJsonOps {
    def (a: AsJson) value: Json = a
  }
}
