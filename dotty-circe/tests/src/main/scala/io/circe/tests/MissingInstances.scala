package io.circe.tests

import io.circe.cats.kernel.Eq
import java.util.UUID
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.util.Buildable

trait MissingInstances {
  implicit lazy val eqThrowable: Eq[Throwable] = Eq.fromUniversalEquals
  implicit lazy val eqBigDecimal: Eq[BigDecimal] = Eq.fromUniversalEquals
  implicit lazy val eqUUID: Eq[UUID] = Eq.fromUniversalEquals
  implicit def eqRefArray[A <: AnyRef: Eq]: Eq[Array[A]] =
    Eq.by((value: Array[A]) => Predef.wrapRefArray(value).toVector)
  implicit def eqSeq[A: Eq]: Eq[Seq[A]] = Eq.by((_: Seq[A]).toVector)

  implicit def arbitraryTuple1[A](implicit A: Arbitrary[A]): Arbitrary[Tuple1[A]] =
    Arbitrary(A.arbitrary.map(Tuple1(_)))

  implicit def arbitrarySome[A](implicit A: Arbitrary[A]): Arbitrary[Some[A]] = Arbitrary(A.arbitrary.map(Some(_)))
  implicit lazy val arbitraryNone: Arbitrary[None.type] = Arbitrary(Gen.const(None))

  implicit def eqSome[A](implicit A: Eq[A]): Eq[Some[A]] = Eq.by(_.get)
  implicit lazy val eqNone: Eq[None.type] = Eq.instance((_, _) => true)

  implicit lazy val arbitrarySymbol: Arbitrary[Symbol] = Arbitrary(Arbitrary.arbitrary[String].map(Symbol(_)))
}