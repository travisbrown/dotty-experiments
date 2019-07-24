package io.circe.cats

/**
 * Commutative Monad.
 *
 * Further than a Monad, which just allows composition of dependent effectful functions,
 * in a Commutative Monad those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeMonadLaws.
 */
trait CommutativeMonad[F[_]] extends Monad[F] with CommutativeFlatMap[F] with CommutativeApplicative[F]

object CommutativeMonad {
	def apply[F[_]] given (F: CommutativeMonad[F]): CommutativeMonad[F] = F

  given as CommutativeMonad[Id] = io.circe.cats.instances.IdInstance
  given as CommutativeMonad[Option] = io.circe.cats.instances.OptionInstance
}