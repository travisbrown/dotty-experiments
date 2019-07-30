package io.circe.cats.kernel

/** ADT encoding the possible results of a comparison */
enum Comparison(val toInt: Int, val toDouble: Double) {
  case GreaterThan extends Comparison(1, 1.0)
  case EqualTo extends Comparison(0, 0.0)
  case LessThan extends Comparison(-1, -1.0)
}

object Comparison {
  // Used for fromDouble
  private val SomeGt = Some(Comparison.GreaterThan)
  private val SomeEq = Some(Comparison.EqualTo)
  private val SomeLt = Some(Comparison.LessThan)

  def fromInt(int: Int): Comparison =
    if (int > 0) Comparison.GreaterThan
    else if (int == 0) Comparison.EqualTo
    else Comparison.LessThan // scalastyle:ignore ensure.single.space.after.token

  def fromDouble(double: Double): Option[Comparison] =
    if (double.isNaN) None
    else if (double > 0.0) SomeGt
    else if (double == 0.0) SomeEq
    else SomeLt

  // Eq.fromUniversalEquals crashes with `java.lang.VerifyError: Bad type on operand stack`.
  given as Eq[Comparison] {
    def eqv(x: Comparison, y: Comparison): Boolean = x == y
  }
}
