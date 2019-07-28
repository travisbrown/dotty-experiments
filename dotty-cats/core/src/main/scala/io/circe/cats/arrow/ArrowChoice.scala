package io.circe.cats.arrow

/**
 * Must obey the laws defined in cats.laws.ArrowChoiceLaws.
 */
trait ArrowChoice[F[_, _]] extends Arrow[F] with Choice[F] { self =>

  /**
   * ArrowChoice yields Arrows with choice, allowing distribution
   * over coproducts.
   *
   * Given two `F`s (`f` and `g`), create a new `F` with
   * domain the coproduct of the domains of `f` and `g`,
   * and codomain the coproduct of the codomains of `f` and `g`.
   * This is the sum notion to `split`'s product.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val toLong: Int => Long = _.toLong
   * scala> val toDouble: Float => Double = _.toDouble
   * scala> val f: Either[Int, Float] => Either[Long, Double] = toLong +++ toDouble
   * scala> f(Left(3))
   * res0: Either[Long,Double] = Left(3)
   * scala> f(Right(3))
   * res1: Either[Long,Double] = Right(3.0)
   * }}}
   */
  def choose[A, B, C, D](f: F[A, C])(g: F[B, D]): F[Either[A, B], Either[C, D]]

  def left[A, B, C](fab: F[A, B]): F[Either[A, C], Either[B, C]] =
    choose(fab)(lift(identity[C]))

  def right[A, B, C](fab: F[A, B]): F[Either[C, A], Either[C, B]] =
    choose(lift(identity[C]))(fab)

  override def choice[A, B, C](f: F[A, C], g: F[B, C]): F[Either[A, B], C] =
    rmap(choose(f)(g))(_.fold(identity, identity))
}

object ArrowChoice {
  def apply[F[_, _]] given (F: ArrowChoice[F]): ArrowChoice[F] = F

  private[cats] trait Ops {
    given [F[_, _], A, B] {
      def (f: F[A, B]) choose[C, D](g: F[C, D]) given (F: ArrowChoice[F]): F[Either[A, C], Either[B, D]] = F.choose(f)(g)
      def (f: F[A, B]) +++[C, D](g: F[C, D]) given (F: ArrowChoice[F]): F[Either[A, C], Either[B, D]] = F.choose(f)(g)
      def (f: F[A, B]) left[C] given (F: ArrowChoice[F]): F[Either[A, C], Either[B, C]] = F.left(f)
      def (f: F[A, B]) right[C] given (F: ArrowChoice[F]): F[Either[C, A], Either[C, B]] = F.right(f)
    }
  }
}
