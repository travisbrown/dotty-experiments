package io.circe.cats.data

import io.circe.cats._
import io.circe.cats.arrow.FunctionK
import io.circe.cats.kernel.{Eq, Monoid, Semigroup}
import io.circe.cats.kernel.laws.discipline.{EqTests, MonoidTests, SemigroupTests, SerializableTests}
import io.circe.cats.laws.discipline._
import io.circe.cats.laws.discipline.arbitrary._
import io.circe.cats.laws.discipline.eq._
import given io.circe.cats.syntax.applicative._
import given io.circe.cats.syntax.eq._
import given io.circe.cats.syntax.writer._
import io.circe.cats.tests.{CatsSuite, ListWrapper}
import org.scalacheck.Test.Parameters

object WriterTSuite extends CatsSuite {
  override def checkConfig: Parameters = Parameters.default.withMaxSize(5)

  type Logged[A] = Writer[ListWrapper[Int], A]

  checkAll("WriterT[Eval, Int, *]", DeferTests[[x] =>> WriterT[Eval, Int, x]].defer[Int])
  checkAll("WriterT[List, Int, Int]", EqTests[WriterT[List, Int, Int]].eqv)
  checkAll("Eq[WriterT[List, Int, Int]]", SerializableTests.serializable(Eq[WriterT[List, Int, Int]]))

  checkAll("WriterT[Show, MiniInt, *]",
           ContravariantTests[[x] =>> WriterT[Show, MiniInt, x]].contravariant[MiniInt, Int, Boolean])
  checkAll("Contravariant[WriterT[Show, Int, Int]]",
           SerializableTests.serializable(Contravariant[[x] =>> WriterT[Show, Int, x]]))

  // check that this resolves
  Eq[Writer[Int, Int]]

  test("double swap is a noop") {
    check1 { w: WriterT[List, Int, Int] =>
      w.swap.swap === w
    }
  }

  test("reset on pure is a noop") {
    check1 { i: Int =>
      val w = Monad[[x] =>> WriterT[List, Int, x]].pure(i)
      w === w.reset
    }
  }

  test("reset consistency") {
    check3 { (i: Int, w1: WriterT[Id, Int, Int], w2: WriterT[Id, Int, Int]) =>
      // if the value is the same, everything should be the same
      w1.map(_ => i).reset === w2.map(_ => i).reset
    }
  }

  test("tell + written is identity") {
    check1 { (i: Int) =>
      WriterT.tell[Id, Int](i).written === i
    }
  }

  test("value + value is identity") {
    check1 { (i: Int) =>
      WriterT.value[Id, Int, Int](i).value === i
    }
  }

  test("valueT + value is identity") {
    check1 { (i: Int) =>
      WriterT.valueT[Id, Int, Int](i).value === i
    }
  }

  test("value + listen + map(_._1) + value is identity") {
    check1 { (i: Int) =>
      i === WriterT.value[Id, Int, Int](i).listen.map(_._1).value
    }
  }

  test("tell + listen + map(_._2) + value is identity") {
    check1 { (i: Int) =>
      i === WriterT.tell[Id, Int](i).listen.map(_._2).value
    }
  }

  test("Writer.pure and WriterT.liftF are consistent") {
    check1 { (i: Int) =>
      val writer: Writer[String, Int] = Writer.value(i)
      val writerT: WriterT[Option, String, Int] = WriterT.liftF(Some(i))
      writerT.run === Some(writer.run)
    }
  }

  test("show") {
    val writerT: WriterT[Id, List[String], String] = WriterT.put("foo")(List("Some log message")) given Applicative[Id]
    assert(writerT.show === "(List(Some log message),foo)")
  }

  test("tell appends to log") {
    val w1: Writer[String, Int] = Writer.value(3)
    val w2 = w1.tell("foo")
    assert(w2 === Writer("foo", 3))
    assert(w2.tell("bar") === Writer("foobar", 3))
  }

  test("tell instantiates a Writer") {
    assert("foo" === Writer.tell("foo").written)
  }

  test("listen returns a tuple of value and log") {
    val w: Writer[String, Int] = Writer("foo", 3)
    assert(w.listen === Writer("foo", (3, "foo")))
  }

  test("mapK consistent with f(value)+pure") {
    val f: List ~> Option = new FunctionK[List, Option] { def apply[X](x: List[X]) = x.headOption }
    check1 { (writert: WriterT[List, String, Int]) =>
      writert.mapK(f) === WriterT(f(writert.run))
    }
  }

  {
    // F has a SemigroupK
    implicit val F: SemigroupK[ListWrapper] = ListWrapper.semigroupK

    checkAll("WriterT[ListWrapper, ListWrapper[Int], *]",
             SemigroupKTests[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]].semigroupK[Int])
    checkAll("SemigroupK[WriterT[ListWrapper, ListWrapper[Int], *]]",
             SerializableTests.serializable(SemigroupK[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]))
  }

  {
    // F has a MonoidK
    implicit val F: MonoidK[ListWrapper] = ListWrapper.monoidK

    SemigroupK[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]

    checkAll("WriterT[ListWrapper, ListWrapper[Int], *]",
             MonoidKTests[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]].monoidK[Int])
    checkAll("MonoidK[WriterT[ListWrapper, ListWrapper[Int], *]]",
             SerializableTests.serializable(MonoidK[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]))
  }

  {
    // F has a Functor and L has no Semigroup
    implicit val F: Functor[ListWrapper] = ListWrapper.functor

    checkAll("WriterT[ListWrapper, ListWrapper[Int], *]",
             FunctorTests[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]].functor[Int, Int, Int])
    checkAll("Functor[WriterT[ListWrapper, ListWrapper[Int], *]]",
             SerializableTests.serializable(Functor[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]))

    checkAll("WriterT[Listwrapper, Int, *]", CoflatMapTests[[x] =>> WriterT[ListWrapper, Int, x]].coflatMap[Int, Int, Int])
    checkAll("WriterT[ListWrapper, Int, *]", SerializableTests.serializable(CoflatMap[[x] =>> WriterT[ListWrapper, Int, x]]))

    // just making sure this resolves; it's tested above
    Functor[[x] =>> WriterT[Id, ListWrapper[Int], x]]

    Functor[[x] =>> Writer[ListWrapper[Int], x]]

    Functor[Logged]

    checkAll("WriterT[ListWrapper, *, *]",
             BifunctorTests[[x, y] =>> WriterT[ListWrapper, x, y]].bifunctor[Int, Int, Int, Int, Int, Int])
    checkAll("Bifunctor[WriterT[ListWrapper, *, *]]",
             SerializableTests.serializable(Bifunctor[[x, y] =>> WriterT[ListWrapper, x, y]]))
  }

  given as SemigroupalTests.Isomorphisms[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]] =
    SemigroupalTests.Isomorphisms.FromInvariant given (WriterT.catsDataCoflatMapForWriterT given ListWrapper.functor)
  //  the[SemigroupalTests.Isomorphisms[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]]

  // We have varying instances available depending on `F` and `L`.
  // We also battle some inference issues with `Id`.
  // Below we go through some gymnastics in order to test both the implicit
  // resolution and the laws of these various instances.
  {
    // F has an Apply and L has a Semigroup
    given as Apply[ListWrapper] = ListWrapper.applyInstance
    given as Semigroup[ListWrapper[Int]] = ListWrapper.semigroup[Int]

    Functor[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], *]",
             ApplyTests[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]].apply[Int, Int, Int])
    checkAll("Apply[WriterT[ListWrapper, ListWrapper[Int], *]]",
             SerializableTests.serializable(Apply[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]))

    Functor[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    Apply[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    CoflatMap[[x] =>> WriterT[Id, ListWrapper[Int], x]]

    Functor[[x] =>> Writer[ListWrapper[Int], x]]
    Apply[[x] =>> Writer[ListWrapper[Int], x]]
    CoflatMap[[x] =>> Writer[ListWrapper[Int], x]]

    Functor[Logged]
    Apply[Logged]
    CoflatMap[Logged]
  }

  {
    // F has a Monad and L has a Semigroup
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val L: Semigroup[ListWrapper[Int]] = ListWrapper.semigroup[Int]

    Functor[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    Apply[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    CoflatMap[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], *] 1",
             FlatMapTests[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]].flatMap[Int, Int, Int])
    checkAll("FlatMap[WriterT[ListWrapper, ListWrapper[Int], *]] 1",
             SerializableTests.serializable(FlatMap[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]))

    Functor[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    Apply[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    FlatMap[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    CoflatMap[[x] =>> WriterT[Id, ListWrapper[Int], x]]

    Functor[[x] =>> Writer[ListWrapper[Int], x]]
    Apply[[x] =>> Writer[ListWrapper[Int], x]]
    FlatMap[[x] =>> Writer[ListWrapper[Int], x]]
    CoflatMap[[x] =>> Writer[ListWrapper[Int], x]]

    Functor[Logged]
    Apply[Logged]
    FlatMap[Logged]
    CoflatMap[Logged]
  }
  {
    // F has a FlatMap and L has a Monoid
    implicit val F: FlatMap[ListWrapper] = ListWrapper.flatMap
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]

    Functor[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    Apply[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    CoflatMap[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], *] 2",
             FlatMapTests[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]].flatMap[Int, Int, Int])
    checkAll("FlatMap[WriterT[ListWrapper, ListWrapper[Int], *]] 2",
             SerializableTests.serializable(FlatMap[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]))

    Functor[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    Apply[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    FlatMap[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    CoflatMap[[x] =>> WriterT[Id, ListWrapper[Int], x]]

    Functor[[x] =>> Writer[ListWrapper[Int], x]]
    Apply[[x] =>> Writer[ListWrapper[Int], x]]
    FlatMap[[x] =>> Writer[ListWrapper[Int], x]]
    CoflatMap[[x] =>> Writer[ListWrapper[Int], x]]

    Functor[Logged]
    Apply[Logged]
    FlatMap[Logged]
    CoflatMap[Logged]
  }

  {
    // F has an Applicative and L has a Monoid
    implicit val F: Applicative[ListWrapper] = ListWrapper.applicative
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]

    Functor[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    Apply[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    CoflatMap[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], *]",
             ApplicativeTests[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]].applicative[Int, Int, Int])
    checkAll("Applicative[WriterT[ListWrapper, ListWrapper[Int], *]]",
             SerializableTests.serializable(Applicative[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]))

    Functor[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    Apply[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    Applicative[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    CoflatMap[[x] =>> WriterT[Id, ListWrapper[Int], x]]

    Functor[[x] =>> Writer[ListWrapper[Int], x]]
    Apply[[x] =>> Writer[ListWrapper[Int], x]]
    Applicative[[x] =>> Writer[ListWrapper[Int], x]]
    CoflatMap[[x] =>> Writer[ListWrapper[Int], x]]

    Functor[Logged]
    Apply[Logged]
    Applicative[Logged]
    CoflatMap[Logged]
  }

  {
    // F has a Monad and L has a Monoid
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]

    Functor[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    Apply[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    Applicative[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    FlatMap[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    CoflatMap[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], *]",
             MonadTests[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]].monad[Int, Int, Int])
    checkAll("Monad[WriterT[ListWrapper, ListWrapper[Int], *], List[String]]",
             SerializableTests.serializable(Monad[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]))

    Functor[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    Apply[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    Applicative[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    FlatMap[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    CoflatMap[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    Monad[[x] =>> WriterT[Id, ListWrapper[Int], x]]

    Functor[[x] =>> Writer[ListWrapper[Int], x]]
    Apply[[x] =>> Writer[ListWrapper[Int], x]]
    Applicative[[x] =>> Writer[ListWrapper[Int], x]]
    FlatMap[[x] =>> Writer[ListWrapper[Int], x]]
    CoflatMap[[x] =>> Writer[ListWrapper[Int], x]]
    Monad[[x] =>> Writer[ListWrapper[Int], x]]

    Functor[Logged]
    Apply[Logged]
    Applicative[Logged]
    FlatMap[Logged]
    CoflatMap[Logged]
    Monad[Logged]
  }

  {
    // F has an Alternative and L has a Monoid
    implicit val F: Alternative[ListWrapper] = ListWrapper.alternative
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]

    Functor[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    Apply[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    Applicative[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    Alternative[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    CoflatMap[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], *]",
             AlternativeTests[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]].alternative[Int, Int, Int])
    checkAll("Alternative[WriterT[ListWrapper, ListWrapper[Int], *]]",
             SerializableTests.serializable(Alternative[[x] =>> WriterT[ListWrapper, ListWrapper[Int], x]]))
  }

  {
    // F[(L, V)] has a monoid
    implicit val FLV: Monoid[ListWrapper[(Int, Int)]] = ListWrapper.monoid[(Int, Int)]

    Monoid[WriterT[ListWrapper, Int, Int]]
    checkAll("WriterT[ListWrapper, Int, Int]", MonoidTests[WriterT[ListWrapper, Int, Int]].monoid)
    checkAll("Monoid[WriterT[ListWrapper, Int, Int]]",
             SerializableTests.serializable(Monoid[WriterT[ListWrapper, Int, Int]]))

    Monoid[Writer[Int, Int]]
    checkAll("Writer[Int, Int]", MonoidTests[Writer[Int, Int]].monoid)
  }

  {
    // F[(L, V)] has a semigroup
    implicit val FLV: Semigroup[ListWrapper[(Int, Int)]] = ListWrapper.semigroup[(Int, Int)]

    Semigroup[WriterT[ListWrapper, Int, Int]]
    checkAll("WriterT[ListWrapper, Int, Int]", SemigroupTests[WriterT[ListWrapper, Int, Int]].semigroup)
    checkAll("Semigroup[WriterT[ListWrapper, Int, Int]]",
             SerializableTests.serializable(Semigroup[WriterT[ListWrapper, Int, Int]]))

    Semigroup[Writer[Int, Int]]
    checkAll("Writer[Int, Int]", SemigroupTests[Writer[Int, Int]].semigroup)
  }

  {
    // F has an Applicative and L has a Monoid
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]
    //implicit val app = WriterT.ApplicativeForWriterT[[x] =>> Validated[String, x], ListWrapper[Int]]

    given as SemigroupalTests.Isomorphisms[[x] =>> WriterT[[y] =>> Validated[String, y], ListWrapper[Int], x]] =
      SemigroupalTests.Isomorphisms.FromInvariant

    //implicit def eq1[A: Eq]: Eq[WriterT[[x] =>> Validated[String, x], ListWrapper[Int], A]] =
    //  WriterT.EqForWriterT[[x] =>> Validated[String, x], ListWrapper[Int], A]
    //implicit val eq2: Eq[EitherT[[x] =>> WriterT[[y] =>> Validated[String, y], ListWrapper[Int], x], String, Int]] =
    //  EitherT.EqForEitherT[[x] =>> WriterT[[y] =>> Validated[String, y], ListWrapper[Int], x], String, Int]

    Functor[[x] =>> WriterT[[y] =>> Validated[String, y], ListWrapper[Int], x]]
    Apply[[x] =>> WriterT[[y] =>> Validated[String, y], ListWrapper[Int], x]]
    Applicative[[x] =>> WriterT[[y] =>> Validated[String, y], ListWrapper[Int], x]]

    checkAll("WriterT[Validated[String, *], ListWrapper[Int], *]",
             ApplicativeTests[[x] =>> WriterT[[y] =>> Validated[String, y], ListWrapper[Int], x]].applicative[Int, Int, Int])
    checkAll(
      "Applicative[WriterT[Validated[String, *], ListWrapper[Int], *]]",
      SerializableTests.serializable(Applicative[[x] =>> WriterT[[y] =>> Validated[String, y], ListWrapper[Int], x]])
    )
  }

  {
    // F has an ApplicativeError and L has a Monoid
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]
    //implicit val appErr: ApplicativeError[[x] =>> WriterT[[y] =>> Validated[String, y], ListWrapper[Int], x], x] = WriterT.catsDataApplicativeErrorForWriterT[[x] =>> Validated[String, x], ListWrapper[Int], x]

    given as SemigroupalTests.Isomorphisms[[x] =>> WriterT[[y] =>> Validated[String, y], ListWrapper[Int], x]] =
      SemigroupalTests.Isomorphisms.FromInvariant

    checkAll(
      "WriterT[Validated[String, *], ListWrapper[Int], *]",
      ApplicativeErrorTests[[x] =>> WriterT[[y] =>> Validated[String, y], ListWrapper[Int], x], String].applicativeError[Int, Int, Int]
    )
    checkAll(
      "ApplicativeError[WriterT[Validated[String, *], ListWrapper[Int], *], Unit]",
      SerializableTests.serializable(ApplicativeError[[x] =>> WriterT[[y] =>> Validated[String, y], ListWrapper[Int], x], String])
    )
  }

  {
    // F has a MonadError and L has a Monoid
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]
    implicit val eq0: Eq[EitherT[[x] =>> WriterT[Option, ListWrapper[Int], x], Unit, Int]] =
      EitherT.catsDataEqForEitherT[[x] =>> WriterT[Option, ListWrapper[Int], x], Unit, Int]

    given as SemigroupalTests.Isomorphisms[[x] =>> WriterT[Option, ListWrapper[Int], x]] =
      SemigroupalTests.Isomorphisms.FromInvariant

    Functor[[x] =>> WriterT[Option, ListWrapper[Int], x]]
    Apply[[x] =>> WriterT[Option, ListWrapper[Int], x]]
    Applicative[[x] =>> WriterT[Option, ListWrapper[Int], x]]
    FlatMap[[x] =>> WriterT[Option, ListWrapper[Int], x]]
    CoflatMap[[x] =>> WriterT[Option, ListWrapper[Int], x]]
    Monad[[x] =>> WriterT[Option, ListWrapper[Int], x]]
    ApplicativeError[[x] =>> WriterT[Option, ListWrapper[Int], x], Unit]

    checkAll("WriterT[Option, ListWrapper[Int], *]",
             MonadErrorTests[[x] =>> WriterT[Option, ListWrapper[Int], x], Unit].monadError[Int, Int, Int])
    checkAll("MonadError[WriterT[Option, ListWrapper[Int], *], Unit]",
             SerializableTests.serializable(MonadError[[x] =>> WriterT[Option, ListWrapper[Int], x], Unit]))
  }

  {
    // F has a ContravariantMonoidal
    ContravariantMonoidal[[x] =>> WriterT[[y] =>> Const[String, y], Int, x]]

    checkAll("WriterT[Const[String, *], Int, *]",
             ContravariantMonoidalTests[[x] =>> WriterT[[y] =>> Const[String, y], Int, x]].contravariantMonoidal[Int, Int, Int])
    checkAll("ContravariantMonoidal[WriterT[Const[String, *], Int, *]]",
             SerializableTests.serializable(ContravariantMonoidal[[x] =>> WriterT[[y] =>> Const[String, y], Int, x]]))
  }

  {
    // F has an Invariant
    implicit val evidence: Invariant[ListWrapper] = ListWrapper.invariant
    Invariant[ListWrapper]
    Invariant[[x] =>> WriterT[ListWrapper, Int, x]]

    checkAll("WriterT[ListWrapper, Int, *]", InvariantTests[[x] =>> WriterT[ListWrapper, Int, x]].invariant[Int, Int, Int])
    checkAll("Invariant[WriterT[ListWrapper, Int, *]]",
             SerializableTests.serializable(Invariant[[x] =>> WriterT[ListWrapper, Int, x]]))
  }
  
  {
    // F has a Foldable and L has a Monoid
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]
    Foldable[[x] =>> Const[String, x]]
    Foldable[[x] =>> WriterT[[y] =>> Const[String, y], ListWrapper[Int], x]]

    checkAll("WriterT[Const[String, *], ListWrapper[Int], *]",
             FoldableTests[[x] =>> WriterT[[y] =>> Const[String, y], ListWrapper[Int], x]].foldable[Int, Int])
    checkAll("Foldable[WriterT[Const[String, *], ListWrapper[Int], *]]",
             SerializableTests.serializable(Foldable[[x] =>> WriterT[[y] =>> Const[String, y], ListWrapper[Int], x]]))

    Foldable[Id]
    Foldable[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    Foldable[[x] =>> Writer[ListWrapper[Int], x]]

    checkAll("WriterT[Id, ListWrapper[Int], *]", FoldableTests[[x] =>> WriterT[Id, ListWrapper[Int], x]].foldable[Int, Int])
  }

  {
    // F has a Traverse and L has a Monoid
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]
    Traverse[[x] =>> Const[String, x]]
    Traverse[[x] =>> WriterT[[y] =>> Const[String, y], ListWrapper[Int], x]]

    checkAll("WriterT[Const[String, *], ListWrapper[Int], *]",
             TraverseTests[[x] =>> WriterT[[y] =>> Const[String, y], ListWrapper[Int], x]].traverse[Int, Int, Int, Int, Option, Option])
    checkAll("Traverse[WriterT[Const[String, *], ListWrapper[Int], *]]",
             SerializableTests.serializable(Traverse[[x] =>> WriterT[[y] =>> Const[String, y], ListWrapper[Int], x]]))

    Traverse[Id]
    Traverse[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    Traverse[[x] =>> Writer[ListWrapper[Int], x]]

    checkAll("WriterT[Id, ListWrapper[Int], *]",
             TraverseTests[[x] =>> WriterT[Id, ListWrapper[Int], x]].traverse[Int, Int, Int, Int, Option, Option])
  }

  {
    // F has a Comonad and L has a Monoid
    Comonad[[x] =>> WriterT[[y] =>> (String, y), ListWrapper[Int], x]]

    checkAll("WriterT[(String, *), ListWrapper[Int], *]",
             ComonadTests[[x] =>> WriterT[[y] =>> (String, y), ListWrapper[Int], x]].comonad[Int, Int, Int])
    checkAll("Comonad[WriterT[(String, *), ListWrapper[Int], *]]",
             SerializableTests.serializable(Comonad[[x] =>> WriterT[[y] =>> (String, y), ListWrapper[Int], x]]))

    Comonad[Id]
    Comonad[[x] =>> WriterT[Id, ListWrapper[Int], x]]
    Comonad[[x] =>> Writer[ListWrapper[Int], x]]

    checkAll("WriterT[Id, ListWrapper[Int], *]", ComonadTests[[x] =>> WriterT[Id, ListWrapper[Int], x]].comonad[Int, Int, Int])
  }

  checkAll("WriterT[Option, Int, *]", CommutativeMonadTests[[x] =>> WriterT[Option, Int, x]].commutativeMonad[Int, Int, Int])
  checkAll("CommutativeMonad[WriterT[Option, Int, *]]",
           SerializableTests.serializable(CommutativeMonad[[x] =>> WriterT[Option, Int, x]]))
}
