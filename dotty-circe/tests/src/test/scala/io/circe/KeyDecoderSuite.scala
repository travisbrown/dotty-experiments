package io.circe

import cats.laws.discipline.MonadErrorTests
import io.circe.tests.CirceSuite

object KeyDecoderSuite extends CirceSuite {
  checkLaws("KeyDecoder[Int]", MonadErrorTests[KeyDecoder, Unit].monadError[Int, Int, Int])
}
