package io.circe.cats

import io.circe.cats.kernel.{Eq, Group, Monoid, Order, PartialOrder, Semigroup}
import scala.annotation.tailrec

/**
 * Eval is a monad which controls evaluation.
 *
 * This type wraps a value (or a computation that produces a value)
 * and can produce it on command via the `.value` method.
 *
 * There are three basic evaluation strategies:
 *
 *  - Now:    evaluated immediately
 *  - Later:  evaluated once when value is needed
 *  - Always: evaluated every time value is needed
 *
 * The Later and Always are both lazy strategies while Now is eager.
 * Later and Always are distinguished from each other only by
 * memoization: once evaluated Later will save the value to be returned
 * immediately if it is needed again. Always will run its computation
 * every time.
 *
 * Eval supports stack-safe lazy computation via the .map and .flatMap
 * methods, which use an internal trampoline to avoid stack overflows.
 * Computation done within .map and .flatMap is always done lazily,
 * even when applied to a Now instance.
 *
 * It is not generally good style to pattern-match on Eval instances.
 * Rather, use .map and .flatMap to chain computation, and use .value
 * to get the result when needed. It is also not good style to create
 * Eval instances whose computation involves calling .value on another
 * Eval instance -- this can defeat the trampolining and lead to stack
 * overflows.
 */
sealed abstract class Eval[+A] extends Serializable { self =>

  /**
   * Evaluate the computation and return an A value.
   *
   * For lazy instances (Later, Always), any necessary computation
   * will be performed at this point. For eager instances (Now), a
   * value will be immediately returned.
   */
  def value: A

  /**
   * Transform an Eval[A] into an Eval[B] given the transformation
   * function `f`.
   *
   * This call is stack-safe -- many .map calls may be chained without
   * consumed additional stack during evaluation.
   *
   * Computation performed in f is always lazy, even when called on an
   * eager (Now) instance.
   */
  def map[B](f: A => B): Eval[B] =
    flatMap(a => Now(f(a)))

  /**
   * Lazily perform a computation based on an Eval[A], using the
   * function `f` to produce an Eval[B] given an A.
   *
   * This call is stack-safe -- many .flatMap calls may be chained
   * without consumed additional stack during evaluation. It is also
   * written to avoid left-association problems, so that repeated
   * calls to .flatMap will be efficiently applied.
   *
   * Computation performed in f is always lazy, even when called on an
   * eager (Now) instance.
   */
  def flatMap[B](f: A => Eval[B]): Eval[B] =
    this match {
      case c: Eval.FlatMap[A] =>
        new Eval.FlatMap[B] {
          type Start = c.Start
          // See https://issues.scala-lang.org/browse/SI-9931 for an explanation
          // of why the type annotations are necessary in these two lines on
          // Scala 2.12.0.
          val start: () => Eval[Start] = c.start
          val run: Start => Eval[B] = (s: c.Start) =>
            new Eval.FlatMap[B] {
              type Start = A
              val start = () => c.run(s)
              val run = f
            }
        }
      case c: Eval.Defer[A] =>
        new Eval.FlatMap[B] {
          type Start = A
          val start = c.thunk
          val run = f
        }
      case _ =>
        new Eval.FlatMap[B] {
          type Start = A
          val start = () => self
          val run = f
        }
    }

  /**
   * Ensure that the result of the computation (if any) will be
   * memoized.
   *
   * Practically, this means that when called on an Always[A] a
   * Later[A] with an equivalent computation will be returned.
   */
  def memoize: Eval[A]
}

/**
 * Construct an eager Eval[A] instance.
 *
 * In some sense it is equivalent to using a val.
 *
 * This type should be used when an A value is already in hand, or
 * when the computation to produce an A value is pure and very fast.
 */
final case class Now[A](value: A) extends Eval[A] {
  def memoize: Eval[A] = this
}

/**
 * Construct a lazy Eval[A] instance.
 *
 * This type should be used for most "lazy" values. In some sense it
 * is equivalent to using a lazy val.
 *
 * When caching is not required or desired (e.g. if the value produced
 * may be large) prefer Always. When there is no computation
 * necessary, prefer Now.
 *
 * Once Later has been evaluated, the closure (and any values captured
 * by the closure) will not be retained, and will be available for
 * garbage collection.
 */
final class Later[A](f: () => A) extends Eval[A] {
  private[this] var thunk: () => A = f

  // The idea here is that `f` may have captured very large
  // structures, but produce a very small result. In this case, once
  // we've calculated a value, we would prefer to be able to free
  // everything else.
  //
  // (For situations where `f` is small, but the output will be very
  // expensive to store, consider using `Always`.)
  lazy val value: A = {
    val result = thunk()
    thunk = null // scalastyle:off
    result
  }

  def memoize: Eval[A] = this
}

object Later {
  def apply[A](a: => A): Later[A] = new Later(() => a)
}

/**
 * Construct a lazy Eval[A] instance.
 *
 * This type can be used for "lazy" values. In some sense it is
 * equivalent to using a Function0 value.
 *
 * This type will evaluate the computation every time the value is
 * required. It should be avoided except when laziness is required and
 * caching must be avoided. Generally, prefer Later.
 */
final class Always[A](f: () => A) extends Eval[A] {
  def value: A = f()
  def memoize: Eval[A] = new Later(f)
}

object Always {
  def apply[A](a: => A): Always[A] = new Always(() => a)
}

object Eval {

  /**
   * Construct an eager Eval[A] value (i.e. Now[A]).
   */
  def now[A](a: A): Eval[A] = Now(a)

  /**
   * Construct a lazy Eval[A] value with caching (i.e. Later[A]).
   */
  def later[A](a: => A): Eval[A] = new Later(() => a)

  /**
   * Construct a lazy Eval[A] value without caching (i.e. Always[A]).
   */
  def always[A](a: => A): Eval[A] = new Always(() => a)

  /**
   * Defer a computation which produces an Eval[A] value.
   *
   * This is useful when you want to delay execution of an expression
   * which produces an Eval[A] value. Like .flatMap, it is stack-safe.
   */
  def defer[A](a: => Eval[A]): Eval[A] =
    new Eval.Defer[A](() => a) {}

  /**
   * Static Eval instance for common value `Unit`.
   *
   * This can be useful in cases where the same value may be needed
   * many times.
   */
  val Unit: Eval[Unit] = Now(())

  /**
   * Static Eval instance for common value `true`.
   *
   * This can be useful in cases where the same value may be needed
   * many times.
   */
  val True: Eval[Boolean] = Now(true)

  /**
   * Static Eval instance for common value `false`.
   *
   * This can be useful in cases where the same value may be needed
   * many times.
   */
  val False: Eval[Boolean] = Now(false)

  /**
   * Static Eval instance for common value `0`.
   *
   * This can be useful in cases where the same value may be needed
   * many times.
   */
  val Zero: Eval[Int] = Now(0)

  /**
   * Static Eval instance for common value `1`.
   *
   * This can be useful in cases where the same value may be needed
   * many times.
   */
  val One: Eval[Int] = Now(1)

  /**
   * Defer is a type of Eval[A] that is used to defer computations
   * which produce Eval[A].
   *
   * Users should not instantiate Defer instances themselves. Instead,
   * they will be automatically created when needed.
   */
  sealed abstract class Defer[A](val thunk: () => Eval[A]) extends Eval[A] {

    def memoize: Eval[A] = Memoize(this)
    def value: A = evaluate(this)
  }

  /**
   * Advance until we find a non-deferred Eval node.
   *
   * Often we may have deep chains of Defer nodes; the goal here is to
   * advance through those to find the underlying "work" (in the case
   * of FlatMap nodes) or "value" (in the case of Now, Later, or
   * Always nodes).
   */
  @tailrec private def advance[A](fa: Eval[A]): Eval[A] =
    fa match {
      case call: Eval.Defer[A] =>
        advance(call.thunk())
      case compute: Eval.FlatMap[A] =>
        new Eval.FlatMap[A] {
          type Start = compute.Start
          val start: () => Eval[Start] = () => compute.start()
          val run: Start => Eval[A] = s => advance1(compute.run(s))
        }
      case other => other
    }

  /**
   * Alias for advance that can be called in a non-tail position
   * from an otherwise tailrec-optimized advance.
   */
  private def advance1[A](fa: Eval[A]): Eval[A] =
    advance(fa)

  /**
   * FlatMap is a type of Eval[A] that is used to chain computations
   * involving .map and .flatMap. Along with Eval#flatMap it
   * implements the trampoline that guarantees stack-safety.
   *
   * Users should not instantiate FlatMap instances
   * themselves. Instead, they will be automatically created when
   * needed.
   *
   * Unlike a traditional trampoline, the internal workings of the
   * trampoline are not exposed. This allows a slightly more efficient
   * implementation of the .value method.
   */
  sealed abstract class FlatMap[A] extends Eval[A] { self =>
    type Start
    val start: () => Eval[Start]
    val run: Start => Eval[A]

    def memoize: Eval[A] = Memoize(this)
    def value: A = evaluate(this)
  }

  private case class Memoize[A](eval: Eval[A]) extends Eval[A] {
    var result: Option[A] = None
    def memoize: Eval[A] = this
    def value: A =
      result match {
        case Some(a) => a
        case None =>
          val a = evaluate(this)
          result = Some(a)
          a
      }
  }

  private def evaluate[A](e: Eval[A]): A = {
    type L = Eval[Any]
    type M = Memoize[Any]
    type C = Any => Eval[Any]

    def addToMemo[X](m: Memoize[X]): X => Eval[X] = { a: X =>
      m.result = Some(a)
      Now(a)
    }

    @tailrec def loop(curr: L, fs: List[C]): Any =
      curr match {
        case c: FlatMap[_] =>
          c.start() match {
            case cc: FlatMap[_] =>
              loop(cc.start().asInstanceOf[L], cc.run.asInstanceOf[C] :: c.run.asInstanceOf[C] :: fs)
            case mm @ Memoize(eval) =>
              mm.result match {
                case Some(a) =>
                  loop(Now(a), c.run.asInstanceOf[C] :: fs)
                case None =>
                  loop(eval, addToMemo(mm.asInstanceOf[M]) :: c.run.asInstanceOf[C] :: fs)
              }
            case xx =>
              loop(c.run(xx.value), fs)
          }
        case call: Defer[_] =>
          loop(advance(call), fs)
        case m @ Memoize(eval) =>
          m.result match {
            case Some(a) =>
              fs match {
                case f :: fs => loop(f(a), fs)
                case Nil     => a
              }
            case None =>
              loop(eval, addToMemo[Any](m.asInstanceOf[Memoize[Any]]) :: fs)
          }
        case x =>
          fs match {
            case f :: fs => loop(f(x.value), fs)
            case Nil     => x.value
          }
      }

    loop(e.asInstanceOf[L], Nil).asInstanceOf[A]
  }

  given as (Bimonad[Eval] & CommutativeMonad[Eval] & io.circe.cats.Defer[Eval] & Reducible[Eval]) = EvalInstance

  private[this] object EvalInstance extends Bimonad[Eval] with StackSafeMonad[Eval] with CommutativeMonad[Eval] with io.circe.cats.Defer[Eval] with Reducible[Eval] {
    override def map[A, B](fa: Eval[A])(f: A => B): Eval[B] = fa.map(f)
    def pure[A](a: A): Eval[A] = Now(a)
    def flatMap[A, B](fa: Eval[A])(f: A => Eval[B]): Eval[B] = fa.flatMap(f)
    def extract[A](la: Eval[A]): A = la.value
    def coflatMap[A, B](fa: Eval[A])(f: Eval[A] => B): Eval[B] = Later(f(fa))

    def defer[A](e: => Eval[A]): Eval[A] = Eval.defer(e)

    def foldLeft[A, B](fa: Eval[A], b: B)(f: (B, A) => B): B =
      f(b, fa.value)
    def foldRight[A, B](fa: Eval[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.flatMap(f(_, lb))

    override def reduce[A](fa: Eval[A]) given (A: Semigroup[A]): A =
      fa.value
    override def reduceLeft[A](fa: Eval[A])(f: (A, A) => A): A =
      fa.value
    def reduceLeftTo[A, B](fa: Eval[A])(f: A => B)(g: (B, A) => B): B =
      f(fa.value)
    override def reduceRight[A](fa: Eval[A])(f: (A, Eval[A]) => Eval[A]): Eval[A] =
      fa
    def reduceRightTo[A, B](fa: Eval[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.map(f)
    override def reduceRightOption[A](fa: Eval[A])(f: (A, Eval[A]) => Eval[A]): Eval[Option[A]] =
      fa.map(Some(_))
    override def reduceRightToOption[A, B](fa: Eval[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
      fa.map { a =>
        Some(f(a))
      }
    override def size[A](f: Eval[A]): Long = 1L
  }

  given [A] as Eq[Eval[A]] given (A: Eq[A]) {
    def eqv(lx: Eval[A], ly: Eval[A]): Boolean =
      A.eqv(lx.value, ly.value)
  }

  given [A] as Order[Eval[A]] given (A: Order[A]) {
    def compare(lx: Eval[A], ly: Eval[A]): Int =
      A.compare(lx.value, ly.value)
  }

  given as Representable[Eval] {
    override type Representation = Unit

    override val F: Functor[Eval] = Functor[Eval]

    /**
     * Create a function that "indexes" into the `F` structure using `Representation`
     */
    override def index[A](f: Eval[A]): Unit => A = (_: Unit) => f.value

    /**
     * Reconstructs the `F` structure using the index function
     */
    override def tabulate[A](f: Unit => A): Eval[A] = Eval.later(f(()))
  }

  given [A] as PartialOrder[Eval[A]] given (A: PartialOrder[A]) {
    def partialCompare(lx: Eval[A], ly: Eval[A]): Double =
      A.partialCompare(lx.value, ly.value)
  }

  given [A] as Semigroup[Eval[A]] given Semigroup[A] =
    new EvalSemigroup[A] {}

  given [A] as Monoid[Eval[A]] given Monoid[A] =
    new EvalMonoid[A] with EvalSemigroup[A] {}

  given [A] as Group[Eval[A]] given Group[A] =
    new EvalGroup[A] with EvalMonoid[A] with EvalSemigroup[A] {}

  private[this] trait EvalSemigroup[A] given (A: Semigroup[A]) extends Semigroup[Eval[A]] {
    def combine(lx: Eval[A], ly: Eval[A]): Eval[A] =
      for { x <- lx; y <- ly } yield A.combine(x, y)
  }

  private[this] trait EvalMonoid[A] given (A: Monoid[A]) extends Monoid[Eval[A]] with EvalSemigroup[A] {
    lazy val empty: Eval[A] = Eval.later(A.empty)
  }

  private[this] trait EvalGroup[A] given (A: Group[A]) extends Group[Eval[A]] with EvalMonoid[A] {
    def inverse(lx: Eval[A]): Eval[A] =
      lx.map(A.inverse)
    override def remove(lx: Eval[A], ly: Eval[A]): Eval[A] =
      for { x <- lx; y <- ly } yield A.remove(x, y)
  }

}
