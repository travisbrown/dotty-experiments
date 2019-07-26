package io.circe

import scala.language.experimental.macros

package object literal {
  implicit final class JsonStringContext(sc: StringContext) {
    inline def json(args: => AsJson*): Json = ${JsonLiteralMacros.jsonStringContext('this, 'args)}
  }
}