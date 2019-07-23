package io.circe.cats.laws.discipline

/**
 * An `ExhaustiveCheck[A]` instance can be used similarly to a ScalaCheck
 * `Gen[A]` instance, but differs in that it generates a `Stream` of the entire
 * domain of values as opposed to generating a random sampling of values.
 */
trait ExhaustiveCheck[A] extends Serializable {
  def allValues: List[A]
}

object ExhaustiveCheck {
  def apply[A] given (A: ExhaustiveCheck[A]): ExhaustiveCheck[A] = A

  def instance[A](values: List[A]): ExhaustiveCheck[A] = new ExhaustiveCheck[A] {
    val allValues: List[A] = values
  }

  given as ExhaustiveCheck[Boolean] = instance(List(false, true))

  given as ExhaustiveCheck[Set[Boolean]] = forSet[Boolean]

  /**
   * Warning: the domain of (A, B) is the cross-product of the domain of `A` and the domain of `B`.
   */
  given [A, B] as ExhaustiveCheck[(A, B)] given (A: ExhaustiveCheck[A],
                                                      B: ExhaustiveCheck[B]) =
    instance(A.allValues.flatMap(a => B.allValues.map(b => (a, b))))

  /**
   * Warning: the domain of (A, B, C) is the cross-product of the 3 domains.
   */
  given Tuple3ExhaustiveCheck[A, B, C] as ExhaustiveCheck[(A, B, C)] given (A: ExhaustiveCheck[A],
                                                         B: ExhaustiveCheck[B],
                                                         C: ExhaustiveCheck[C]) =
    instance(
      for {
        a <- A.allValues
        b <- B.allValues
        c <- C.allValues
      } yield (a, b, c)
    )

  given [A, B] as ExhaustiveCheck[Either[A, B]] given (A: ExhaustiveCheck[A], B: ExhaustiveCheck[B]) =
    instance(A.allValues.map(Left(_)) ++ B.allValues.map(Right(_)))

  given [A] as ExhaustiveCheck[Option[A]] given (A: ExhaustiveCheck[A]) =
    instance(None :: A.allValues.map(Some(_)))

  /**
   * Creates an `ExhaustiveCheck[Set[A]]` given an `ExhaustiveCheck[A]` by computing the powerset of
   * values. Note that if there are `n` elements in the domain of `A` there will be `2^n` elements
   * in the domain of `Set[A]`, so use this only on small domains.
   */
  def forSet[A] given (A: ExhaustiveCheck[A]): ExhaustiveCheck[Set[A]] =
    instance(A.allValues.toSet.subsets.toList)
}
