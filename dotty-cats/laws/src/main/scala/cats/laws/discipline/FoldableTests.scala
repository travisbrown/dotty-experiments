package io.circe.cats.laws.discipline

import io.circe.cats.Foldable
import io.circe.cats.kernel.{CommutativeMonoid, Eq}
import io.circe.cats.laws.FoldableLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait FoldableTests[F[_]] extends UnorderedFoldableTests[F] {
  def laws: FoldableLaws[F]

  def foldable[A, B] given Arbitrary[A], Arbitrary[B], Arbitrary[F[A]], Cogen[A], Cogen[B], CommutativeMonoid[A], CommutativeMonoid[B], Eq[A], Eq[B], Eq[F[A]], Eq[Option[A]], Eq[Option[B]]: RuleSet =
    new DefaultRuleSet(
      name = "foldable",
      parent = Some(unorderedFoldable[A, B]),
      "foldLeft consistent with foldMap" -> Prop.forAll(laws.leftFoldConsistentWithFoldMap[A, B]),
      "foldRight consistent with foldMap" -> Prop.forAll(laws.rightFoldConsistentWithFoldMap[A, B]),
      "foldRight is lazy" -> Prop.forAll(laws.foldRightLazy[A]),
      "ordered constistency" -> Prop.forAll(laws.orderedConsistency[A]),
      "exists consistent with find" -> Prop.forAll(laws.existsConsistentWithFind[A]),
      "foldM identity" -> Prop.forAll(laws.foldMIdentity[A, B]),
      "reduceLeftOption consistent with reduceLeftToOption" ->
        Prop.forAll(laws.reduceLeftOptionConsistentWithReduceLeftToOption[A]),
      "reduceRightOption consistent with reduceRightToOption" ->
        Prop.forAll(laws.reduceRightOptionConsistentWithReduceRightToOption[A]),
      "get reference" -> Prop.forAll(laws.getRef[A]),
      "fold reference" -> Prop.forAll(laws.foldRef[A]),
      "toList reference" -> Prop.forAll(laws.toListRef[A]),
      "filter_ reference" -> Prop.forAll(laws.filter_Ref[A]),
      "takeWhile_ reference" -> Prop.forAll(laws.takeWhile_Ref[A]),
      "dropWhile_ reference" -> Prop.forAll(laws.dropWhile_Ref[A]),
      "collectFirstSome reference" -> Prop.forAll(laws.collectFirstSome_Ref[A, B]),
      "collectFirst reference" -> Prop.forAll(laws.collectFirst_Ref[A, B])
    )
}

object FoldableTests {
  def apply[F[_]] given Foldable[F]: FoldableTests[F] =
    new FoldableTests[F] { def laws: FoldableLaws[F] = FoldableLaws[F] }
}
