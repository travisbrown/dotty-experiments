package io.circe.cats

import java.util.UUID
import scala.collection.immutable.{BitSet, SortedMap, SortedSet}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.language.implicitConversions

/**
 * A type class to provide textual representation. It is meant to be a
 * better "toString". Whereas toString exists for any Object,
 * regardless of whether or not the creator of the class explicitly
 * made a toString method, a Show instance will only exist if someone
 * explicitly provided one.
 */
trait Show[T] extends Show.ContravariantShow[T]

/**
 * Hand rolling the type class boilerplate due to scala/bug#6260 and scala/bug#10458
 */
object Show {

  def apply[A] given (A: Show[A]): Show[A] = A

  trait ContravariantShow[-T] extends Serializable {
    def show(t: T): String
  }

  /*trait Ops[A] {
    def typeClassInstance: Show[A]
    def self: A
    def show: String = typeClassInstance.show(self)
  }

  trait ToShowOps {
    implicit def toShow[A](target: A)(implicit tc: Show[A]): Ops[A] = new Ops[A] {
      val self = target
      val typeClassInstance = tc
    }
  }*/

  /** creates an instance of [[Show]] using the provided function */
  def show[A](f: A => String): Show[A] = new Show[A] {
    def show(a: A): String = f(a)
  }

  /** creates an instance of [[Show]] using object toString */
  def fromToString[A]: Show[A] = new Show[A] {
    def show(a: A): String = a.toString
  }

  final case class Shown(override val toString: String) extends AnyVal

  object Shown {
    implicit def mat[A](x: A)(implicit z: ContravariantShow[A]): Shown = Shown(z.show(x))
  }

  final case class ShowInterpolator(_sc: StringContext) extends AnyVal {
    def show(args: Shown*): String = _sc.s(args: _*)
  }

  given as Contravariant[Show] {
    def contramap[A, B](fa: Show[A])(f: B => A): Show[B] =
      show[B]((fa.show _).compose(f))
  }

  given as Show[Unit] = Show.fromToString[Unit]
  given as Show[Boolean] = Show.fromToString[Boolean]
  given as Show[Byte] = Show.fromToString[Byte]
  given as Show[Short] = Show.fromToString[Short]
  given as Show[Int] = Show.fromToString[Int]
  given as Show[Long] = Show.fromToString[Long]
  given as Show[Float] = Show.fromToString[Float]
  given as Show[Double] = Show.fromToString[Double]
  given as Show[BigInt] = Show.fromToString[BigInt]
  given as Show[BigDecimal] = Show.fromToString[BigDecimal]
  given as Show[Char] = Show.fromToString[Char]
  given as Show[Symbol] = Show.fromToString[Symbol]
  given as Show[String] = Show.fromToString[String]
  given as Show[UUID] = Show.fromToString[UUID]
  given as Show[Duration] = Show.fromToString[Duration]
  given as Show[FiniteDuration] = Show.fromToString[FiniteDuration]
  given as Show[BitSet] = Show.fromToString[BitSet]

  given [A] as Show[Option[A]] given (A: Show[A]) {
    def show(fa: Option[A]): String = fa match {
      case Some(a) => s"Some(${A.show(a)})"
      case None    => "None"
    }
  }

  given [A] as Show[List[A]] given (A: Show[A]) {
    def show(fa: List[A]): String =
      fa.iterator.map(A.show).mkString("List(", ", ", ")")
  }

  given [A] as Show[Vector[A]] given (A: Show[A]) {
    def show(fa: Vector[A]): String =
      fa.iterator.map(A.show).mkString("Vector(", ", ", ")")
  }

  given [A] as Show[Stream[A]] given (A: Show[A]) {
    def show(fa: Stream[A]): String = if (fa.isEmpty) "Stream()" else s"Stream(${A.show(fa.head)}, ?)"
  }

  given [A] as Show[Set[A]] given (A: Show[A]) {
    def show(fa: Set[A]): String =
      fa.iterator.map(A.show).mkString("Set(", ", ", ")")
  }

  given [A, B] as Show[Either[A, B]] given (A: Show[A], B: Show[B]) {
    def show(x: Either[A, B]): String =
      x match {
        case Left(a)  => "Left(" + A.show(a) + ")"
        case Right(b) => "Right(" + B.show(b) + ")"
      }
  }

  given [A] as Show[SortedSet[A]] given (A: Show[A]) {
    def show(fa: SortedSet[A]): String =
      fa.iterator.map(A.show).mkString("SortedSet(", ", ", ")")
  }

  given [K, V] as Show[SortedMap[K, V]] given (K: Show[K], V: Show[V]) {
    def show(m: SortedMap[K, V]): String =
      m.iterator
        .map { case (k, v) => K.show(k) + " -> " + V.show(v) }
        .mkString("SortedMap(", ", ", ")")
  }

  given [K, V] as Show[Map[K, V]] given (K: Show[K], V: Show[V]) {
    def show(m: Map[K, V]): String =
      m.iterator
        .map { case (k, v) => K.show(k) + " -> " + V.show(v) }
        .mkString("Map(", ", ", ")")
  }

  given [A, B] as Show[(A, B)] given (A: Show[A], B: Show[B]) {
    def show(f: (A, B)): String = s"(${A.show(f._1)},${B.show(f._2)})"
  }

  private[cats] trait Ops {
    given [A] {
      def (a: A) show given (A: Show[A]): String = A.show(a)
    }
  }
}
