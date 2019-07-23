package io.circe.cats.laws.discipline

import io.circe.cats.{Applicative, CommutativeApplicative, NonEmptyTraverse}
import io.circe.cats.kernel.{CommutativeMonoid, Eq}
import io.circe.cats.laws.NonEmptyTraverseLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait NonEmptyTraverseTests[F[_]] extends TraverseTests[F] with ReducibleTests[F] {
  def laws: NonEmptyTraverseLaws[F]

  def nonEmptyTraverse[G[_], A, B, C, M, X[_], Y[_]] given Applicative[G],
    Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[M],
    CommutativeApplicative[X], CommutativeApplicative[Y],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[X[B]], Arbitrary[X[M]], Arbitrary[Y[B]], Arbitrary[Y[C]], Arbitrary[Y[M]],
    Arbitrary[F[G[A]]], Arbitrary[F[X[M]]], Arbitrary[G[B]], Arbitrary[G[M]],
    Cogen[A], Cogen[B], Cogen[C], Cogen[M],
    CommutativeMonoid[A], CommutativeMonoid[B], CommutativeMonoid[M],
    Eq[A], Eq[B], Eq[M], Eq[F[A]], Eq[F[C]], Eq[X[Y[F[C]]]], Eq[X[F[B]]], Eq[Y[F[B]]], Eq[X[F[M]]], Eq[Y[F[M]]], Eq[G[Unit]], Eq[Option[A]]: RuleSet = {
    given as Eq[(X[F[B]], Y[F[B]])] {
      override def eqv(x: (X[F[B]], Y[F[B]]), y: (X[F[B]], Y[F[B]])): Boolean =
        the[Eq[X[F[B]]]].eqv(x._1, y._1) && the[Eq[Y[F[B]]]].eqv(x._2, y._2)
    }
    new RuleSet {
      def name: String = "nonEmptyTraverse"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(traverse[A, B, C, M, X, Y], reducible[G, A, B])
      def props: Seq[(String, Prop)] = Seq(
        "nonEmptyTraverse identity" -> Prop.forAll(laws.nonEmptyTraverseIdentity[A, C]),
        "nonEmptyTraverse sequential composition" -> Prop.forAll(
          laws.nonEmptyTraverseSequentialComposition[A, B, C, X, Y]
        ),
        "nonEmptyTraverse parallel composition" -> Prop.forAll(laws.nonEmptyTraverseParallelComposition[A, B, X, Y]),
        "nonEmptyTraverse derive reduceMap" -> Prop.forAll(laws.reduceMapDerived[A, M])
      )
    }
  }
}

object NonEmptyTraverseTests {
  def apply[F[_]] given NonEmptyTraverse[F]: NonEmptyTraverseTests[F] =
    new NonEmptyTraverseTests[F] { def laws: NonEmptyTraverseLaws[F] = NonEmptyTraverseLaws[F] }
}
