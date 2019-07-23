package io.circe

import cats.ApplicativeError
import cats.data.NonEmptyList
import cats.kernel.Semigroup

@deprecated("Use Decoder", "0.12.0")
final object AccumulatingDecoder {
  @deprecated("Use Decoder.AccumulatingResult", "0.12.0")
  final type Result[A] = Decoder.AccumulatingResult[A]

  @deprecated("Use NonEmptyList.catsDataSemigroupForNonEmptyList[DecodingFailure]", "0.12.0")
  final val failureNelInstance: Semigroup[NonEmptyList[DecodingFailure]] =
    NonEmptyList.catsDataSemigroupForNonEmptyList[DecodingFailure]

  @deprecated("Use Decoder.accumulatingResultInstance", "0.12.0")
  final val resultInstance: ApplicativeError[Result, NonEmptyList[DecodingFailure]] =
    Decoder.accumulatingResultInstance
}
