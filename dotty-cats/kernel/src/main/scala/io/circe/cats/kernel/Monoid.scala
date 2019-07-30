package io.circe.cats.kernel

/**
 * A monoid is a semigroup with an identity. A monoid is a specialization of a
 * semigroup, so its operation must be associative. Additionally,
 * `combine(x, empty) == combine(empty, x) == x`. For example, if we have `Monoid[String]`,
 * with `combine` as string concatenation, then `empty = ""`.
 */
trait Monoid[@specialized(Int, Long, Float, Double) A] extends Any with Semigroup[A] {

  /**
   * Return the identity element for this monoid.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.int._
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].empty
   * res0: String = ""
   *
   * scala> Monoid[Int].empty
   * res1: Int = 0
   * }}}
   */
  def empty: A

  /**
   * Tests if `a` is the identity.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].isEmpty("")
   * res0: Boolean = true
   *
   * scala> Monoid[String].isEmpty("something")
   * res1: Boolean = false
   * }}}
   */
  def isEmpty(a: A) given (A: Eq[A]): Boolean =
    A.eqv(a, empty)

  /**
   * Return `a` appended to itself `n` times.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].combineN("ha", 3)
   * res0: String = hahaha
   *
   * scala> Monoid[String].combineN("ha", 0)
   * res1: String = ""
   * }}}
   */
  override def combineN(a: A, n: Int): A =
    if (n < 0) throw new IllegalArgumentException("Repeated combining for monoids must have n >= 0")
    else if (n == 0) empty
    else repeatedCombineN(a, n)

  /**
   * Given a sequence of `as`, sum them using the monoid and return the total.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].combineAll(List("One ", "Two ", "Three"))
   * res0: String = One Two Three
   *
   * scala> Monoid[String].combineAll(List.empty)
   * res1: String = ""
   * }}}
   */
  def combineAll(as: TraversableOnce[A]): A =
    as.toIterator.foldLeft(empty)(combine)

  override def combineAllOption(as: TraversableOnce[A]): Option[A] =
    if (as.toIterator.isEmpty) None else Some(combineAll(as))
}

private[kernel] abstract class MonoidFunctions[M[T] <: Monoid[T]] extends SemigroupFunctions[M] {
  def empty[@specialized(Int, Long, Float, Double) A] given (A: M[A]): A =
    A.empty

  def isEmpty[@specialized(Int, Long, Float, Double) A](a: A) given M[A], Eq[A]: Boolean =
    the[M[A]].isEmpty(a)

  def combineAll[@specialized(Int, Long, Float, Double) A](as: TraversableOnce[A]) given (A: M[A]): A =
    A.combineAll(as)
}

object Monoid extends MonoidFunctions[Monoid] {

  /**
   * Access a given `Monoid[A]`.
   */
  def apply[A] given (A: Monoid[A]): Monoid[A] = A

  /**
   * Create a `Monoid` instance from the given function and empty value.
   */
  def instance[A](emptyValue: A, cmb: (A, A) => A): Monoid[A] = new Monoid[A] {
    override val empty: A = emptyValue

    override def combine(x: A, y: A): A = cmb(x, y)
  }

  import scala.compiletime.{erasedValue, error}
  import scala.deriving.{Mirror, productElement}

  inline def derived[A] given (A: Mirror.Of[A]): Monoid[A] =
    new Monoid[A] {
      val empty: A =
        inline A match {
          case m: Mirror.ProductOf[A] =>
            m.fromProduct(emptyElems[m.MirroredElemTypes].asInstanceOf[Product])
        }
      def combine(x: A, y: A): A =
        inline A match {
          case m: Mirror.ProductOf[A] =>
            m.fromProduct(combineElems[m.MirroredElemTypes](x, y, 0).asInstanceOf[Product])
        }
    }

  inline def tryMonoid[A]: Monoid[A] = given match {
    case monoidElem: Monoid[A] => monoidElem
    case _ => error("No given `Monoid` was found for A")
  }

  inline def emptyElems[Elems <: Tuple]: Tuple =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        (tryMonoid[elem].empty *: emptyElems[elems1])
      case _: Unit => ()
    }

  inline def combineElems[Elems <: Tuple](x: Any, y: Any, n: Int): Tuple =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        val result = tryMonoid[elem].combine(productElement[elem](x, n), productElement[elem](y, n))

        (result *: combineElems[elems1](x, y, n + 1))
      case _: Unit => ()
    }
}
