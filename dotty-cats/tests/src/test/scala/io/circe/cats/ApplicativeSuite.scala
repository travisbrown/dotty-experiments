package io.circe.cats

import io.circe.cats.data.{Const, Validated}
import io.circe.cats.kernel.{Monoid, Semigroup}
import io.circe.cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import io.circe.cats.laws.discipline.CoflatMapTests
import io.circe.cats.laws.discipline.arbitrary._
import given io.circe.cats.syntax.applicative._
import given io.circe.cats.syntax.eq._
import io.circe.cats.tests.{CatsSuite, ListWrapper}

object ApplicativeSuite extends CatsSuite {

  test("replicateA creates a List of 'n' copies of given Applicative 'fa'") {
    val A = Applicative[Option]
    val fa = A.pure(1)
    assert(fa.replicateA(5) === Some(List(1, 1, 1, 1, 1)))
  }

  test("whenA return given argument when cond is true") {
    check1 { (l: List[Int]) =>
      l.whenA(true) === List.fill(l.length)(())
    }
  }

  test("whenA lift Unit to F when cond is false") {
    check1 { (l: List[Int]) =>
      l.whenA(false) === List(())
    }
  }

  test("unlessA return given argument when cond is false") {
    check1 { (l: List[Int]) =>
      l.unlessA(false) === List.fill(l.length)(())
    }
  }

  test("unlessA lift Unit to F when cond is true") {
    check1 { (l: List[Int]) =>
      l.unlessA(true) === List(())
    }
  }

  {
    implicit val optionMonoid: Monoid[Option[Int]] = Applicative.monoid[Option, Int]
    checkAll("Applicative[Option].monoid", (MonoidTests[Option[Int]] given optionMonoid).monoid)
  }

  {
    val optionSemigroupFromApply: Semigroup[Option[Int]] = Apply.semigroup[Option, Int]
    checkAll("Apply[Option].semigroup", (SemigroupTests[Option[Int]] given optionSemigroupFromApply).semigroup)
  }

  {
    implicit val listwrapperApplicative: Applicative[ListWrapper] = ListWrapper.applicative
    implicit val listwrapperCoflatMap: CoflatMap[ListWrapper] = Applicative.coflatMap[ListWrapper]
    checkAll("Applicative[ListWrapper].coflatMap", CoflatMapTests[ListWrapper].coflatMap[String, String, String])

    implicit val validatedCoflatMap: CoflatMap[[x] =>> Validated[String, x]] = Applicative.coflatMap[[x] =>> Validated[String, x]]
    checkAll("Applicative[Validated].coflatMap", CoflatMapTests[[x] =>> Validated[String, x]].coflatMap[String, String, String])

    implicit val constCoflatMap: CoflatMap[[x] =>> Const[String, x]] = Applicative.coflatMap[[x] =>> Const[String, x]]
    checkAll("Applicative[Const].coflatMap", CoflatMapTests[[x] =>> Const[String, x]].coflatMap[String, String, String])
  }

}
