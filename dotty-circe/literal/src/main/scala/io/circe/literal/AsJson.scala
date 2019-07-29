package io.circe.literal

import io.circe.{Encoder, Json}
import scala.language.implicitConversions

opaque type AsJson = Json

object AsJson {
  // See https://github.com/lampepfl/dotty/issues/6914
  implicit def toAsJson[A](a: A) given (A: Encoder[A]): AsJson = A(a)

  given AsJsonOps {
    def (a: AsJson) value: Json = a
  }
}
