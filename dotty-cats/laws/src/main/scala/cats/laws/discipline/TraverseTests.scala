package io.circe.cats.laws.discipline

import io.circe.cats.{CommutativeApplicative, Traverse}
import io.circe.cats.kernel.{CommutativeMonoid, Eq}
import io.circe.cats.laws.TraverseLaws
import io.circe.cats.laws.discipline.arbitrary._
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait TraverseTests[F[_]] extends FunctorTests[F] with FoldableTests[F] with UnorderedTraverseTests[F] {
  def laws: TraverseLaws[F]

  def traverse[A, B, C, M, X[_], Y[_]] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[M],
    CommutativeApplicative[X], CommutativeApplicative[Y],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[X[B]], Arbitrary[X[M]], Arbitrary[Y[B]], Arbitrary[Y[C]], Arbitrary[Y[M]], Arbitrary[F[X[M]]],
    Cogen[A], Cogen[B], Cogen[C], Cogen[M],
    CommutativeMonoid[A], CommutativeMonoid[M],
    Eq[A], Eq[M], Eq[F[A]], Eq[F[C]], Eq[X[Y[F[C]]]], Eq[X[F[B]]], Eq[Y[F[B]]], Eq[X[F[M]]], Eq[Y[F[M]]], Eq[Option[A]]: RuleSet = {
    given as Eq[(X[F[B]], Y[F[B]])] {
      def eqv(x: (X[F[B]], Y[F[B]]), y: (X[F[B]], Y[F[B]])): Boolean =
        the[Eq[X[F[B]]]].eqv(x._1, y._1) && the[Eq[Y[F[B]]]].eqv(x._2, y._2)
    }
    new RuleSet {
      def name: String = "traverse"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(functor[A, B, C], foldable[A, M], unorderedTraverse[A, M, C, X, Y])
      def props: Seq[(String, Prop)] = Seq(
        "traverse identity" -> Prop.forAll(laws.traverseIdentity[A, C]),
        "traverse sequential composition" -> Prop.forAll(laws.traverseSequentialComposition[A, B, C, X, Y]),
        "traverse parallel composition" -> Prop.forAll(laws.traverseParallelComposition[A, B, X, Y]),
        "traverse derive foldMap" -> Prop.forAll(laws.foldMapDerived[A, M]),
        "traverse order consistency" -> Prop.forAll(laws.traverseOrderConsistent[A]),
        "traverse ref mapWithIndex" -> Prop.forAll(laws.mapWithIndexRef[A, C]),
        "traverse ref traverseWithIndexM" -> Prop.forAll(laws.traverseWithIndexMRef[Option, A, C]),
        "traverse ref zipWithIndex" -> Prop.forAll(laws.zipWithIndexRef[A, C])
      )
    }
  }
}

object TraverseTests {
  def apply[F[_]] given Traverse[F]: TraverseTests[F] =
    new TraverseTests[F] { def laws: TraverseLaws[F] = TraverseLaws[F] }
}
