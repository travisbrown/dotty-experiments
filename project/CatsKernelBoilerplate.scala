import CatsKernelBoilerplate.TemplateVals
import sbt._

/**
 * Generate a range of boilerplate classes that would be tedious to write and maintain by hand.
 *
 * Copied, with some modifications, from
 * [[https://github.com/milessabin/shapeless/blob/master/project/Boilerplate.scala Shapeless]].
 *
 * @author Miles Sabin
 * @author Kevin Wright
 */
object CatsKernelBoilerplate {
  import scala.StringContext._

  implicit class BlockHelper(private val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated.split('\n')
      val trimmedLines = rawLines.map(_.dropWhile(_.isWhitespace))
      trimmedLines.mkString("\n")
    }
  }

  val templates: Seq[Template] = Seq(GenTupleInstances)

  val header = "// auto-generated boilerplate"
  val maxArity = 22

  /**
   * Return a sequence of the generated files.
   *
   * As a side-effect, it actually generates them...
   */
  def gen(dir: File): Seq[File] = templates.map { template =>
    val tgtFile = template.filename(dir)
    IO.write(tgtFile, template.body)
    tgtFile
  }

  class TemplateVals(val arity: Int) {
    val synTypes = (0 until arity).map(n => s"A$n")
    val synVals = (0 until arity).map(n => s"a$n")
    val `A..N` = synTypes.mkString(", ")
    val `a..n` = synVals.mkString(", ")
    val `_.._` = Seq.fill(arity)("_").mkString(", ")
    val `(A..N)` = if (arity == 1) "Tuple1[A0]" else synTypes.mkString("(", ", ", ")")
    val `(_.._)` = if (arity == 1) "Tuple1[_]" else Seq.fill(arity)("_").mkString("(", ", ", ")")
    val `(a..n)` = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
  }

  /**
   * Blocks in the templates below use a custom interpolator, combined with post-processing to
   * produce the body.
   *
   * - The contents of the `header` val is output first
   * - Then the first block of lines beginning with '|'
   * - Then the block of lines beginning with '-' is replicated once for each arity,
   *   with the `templateVals` already pre-populated with relevant relevant vals for that arity
   * - Then the last block of lines prefixed with '|'
   *
   * The block otherwise behaves as a standard interpolated string with regards to variable
   * substitution.
   */
  trait Template {
    def filename(root: File): File
    def preBody: String
    def instances: Seq[InstanceDef]
    def range: IndexedSeq[Int] = 1 to maxArity
    def body: String = {
      val headerLines = header.split('\n')
      val tvs = range.map(n => new TemplateVals(n))
      (headerLines ++ Seq(preBody) ++ instances.flatMap(_.body(tvs))).mkString("\n")
    }
  }

  case class InstanceDef(start: String, methods: TemplateVals => TemplatedBlock, end: String = "}") {
    def body(tvs: Seq[TemplateVals]): Seq[String] = Seq(start) ++ tvs.map(methods(_).content) ++ Seq(end)
  }

  abstract class TemplatedBlock(tv: TemplateVals) {
    import tv._

    def constraints(constraint: String) =
      synTypes.map(tpe => s"${tpe}: ${constraint}[${tpe}]").mkString(", ")

    def tuple(results: TraversableOnce[String]) = {
      val resultsVec = results.toVector
      val a = synTypes.size
      val r = s"${0.until(a).map(i => resultsVec(i)).mkString(", ")}"
      if (a == 1) "Tuple1(" ++ r ++ ")"
      else s"(${r})"
    }

    def tupleNHeader = s"Tuple${synTypes.size}"

    def binMethod(name: String) =
      synTypes.zipWithIndex.iterator.map {
        case (tpe, i) =>
          val j = i + 1
          s"${tpe}.${name}(x._${j}, y._${j})"
      }

    def binTuple(name: String) =
      tuple(binMethod(name))

    def unaryTuple(name: String) = {
      val m = synTypes.zipWithIndex.map { case (tpe, i) => s"${tpe}.${name}(x._${i + 1})" }
      tuple(m)
    }

    def unaryMethod(name: String) =
      synTypes.zipWithIndex.iterator.map {
        case (tpe, i) =>
          s"$tpe.$name(x._${i + 1})"
      }

    def nullaryTuple(name: String) = {
      val m = synTypes.map(tpe => s"${tpe}.${name}")
      tuple(m)
    }

    def content: String
  }

  object GenTupleInstances extends Template {
    override def range: IndexedSeq[Int] = 1 to maxArity

    def filename(root: File): File = root / "cats" / "kernel" / "instances" / "TupleInstances.scala"

    val preBody: String =
      block"""
         package io.circe.cats.kernel.instances

         import io.circe.cats.kernel.{Band, BoundedSemilattice, CommutativeGroup, CommutativeMonoid, CommutativeSemigroup, Eq, Group, Hash, Monoid, Order, PartialOrder, Semigroup, Semilattice}
     """

    def instances: Seq[InstanceDef] =
      Seq(
        InstanceDef(
          "private[kernel] trait TupleBandInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                  given Tuple${arity}Band[${`A..N`}] as Band[${`(A..N)`}] given (${constraints("Band")}) {
                    def combine(x: ${`(A..N)`}, y: ${`(A..N)`}): ${`(A..N)`} = ${binTuple("combine")}
                  }
                """
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleBoundedSemilatticeInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                  given Tuple${arity}BoundedSemilattice[${`A..N`}] as BoundedSemilattice[${`(A..N)`}] given (${constraints(
                  "BoundedSemilattice"
                )}) {
                    def combine(x: ${`(A..N)`}, y: ${`(A..N)`}): ${`(A..N)`} = ${binTuple("combine")}
                    def empty: ${`(A..N)`} = ${nullaryTuple("empty")}
                  }
                """
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleCommutativeGroupInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                  given Tuple${arity}CommutativeGroup[${`A..N`}] as CommutativeGroup[${`(A..N)`}] given (${constraints(
                  "CommutativeGroup"
                )}) {
                    def combine(x: ${`(A..N)`}, y: ${`(A..N)`}): ${`(A..N)`} = ${binTuple("combine")}
                    def empty: ${`(A..N)`} = ${nullaryTuple("empty")}
                    def inverse(x: ${`(A..N)`}): ${`(A..N)`} = ${unaryTuple("inverse")}
                  }
                """
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleCommutativeMonoidInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                  given Tuple${arity}CommutativeMonoid[${`A..N`}] as CommutativeMonoid[${`(A..N)`}] given (${constraints(
                  "CommutativeMonoid"
                )}) {
                    def combine(x: ${`(A..N)`}, y: ${`(A..N)`}): ${`(A..N)`} = ${binTuple("combine")}
                    def empty: ${`(A..N)`} = ${nullaryTuple("empty")}
                  }
                """
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleCommutativeSemigroupInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                  given Tuple${arity}CommutativeSemigroup[${`A..N`}] as CommutativeSemigroup[${`(A..N)`}] given (${constraints(
                  "CommutativeSemigroup"
                )}) {
                    def combine(x: ${`(A..N)`}, y: ${`(A..N)`}): ${`(A..N)`} = ${binTuple("combine")}
                  }
                """
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleEqInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                  given Tuple${arity}Eq[${`A..N`}] as Eq[${`(A..N)`}] given (${constraints("Eq")}) {
                    def eqv(x: ${`(A..N)`}, y: ${`(A..N)`}): Boolean = ${binMethod("eqv").mkString(" && ")}
                  }
                """
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleGroupInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                  given Tuple${arity}Group[${`A..N`}] as Group[${`(A..N)`}] given (${constraints("Group")}) {
                    def combine(x: ${`(A..N)`}, y: ${`(A..N)`}): ${`(A..N)`} = ${binTuple("combine")}
                    def empty: ${`(A..N)`} = ${nullaryTuple("empty")}
                    def inverse(x: ${`(A..N)`}): ${`(A..N)`} = ${unaryTuple("inverse")}
                  }
                """
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleHashInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                given Tuple${arity}Hash[${`A..N`}] as Hash[${`(A..N)`}] given (${constraints("Hash")}) {
                  def hash(x: ${`(A..N)`}): Int = ${unaryMethod("hash")
                  .mkString(s"$tupleNHeader(", ", ", ")")}.hashCode()
                  def eqv(x: ${`(A..N)`}, y: ${`(A..N)`}): Boolean = ${binMethod("eqv").mkString(" && ")}
                }
                """
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleMonoidInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                  given Tuple${arity}Monoid[${`A..N`}] as Monoid[${`(A..N)`}] given (${constraints("Monoid")}) {
                    def combine(x: ${`(A..N)`}, y: ${`(A..N)`}): ${`(A..N)`} = ${binTuple("combine")}
                    def empty: ${`(A..N)`} = ${nullaryTuple("empty")}
                  }
                """
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleOrderInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                  given Tuple${arity}Order[${`A..N`}] as Order[${`(A..N)`}] given (${constraints("Order")}) {
                    def compare(x: ${`(A..N)`}, y: ${`(A..N)`}): Int =
                      ${binMethod("compare").mkString("Array(", ", ", ")")}.find(_ != 0).getOrElse(0)
                  }
                """
            }
        ),
        InstanceDef(
          "private[kernel] trait TuplePartialOrderInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                  given Tuple${arity}PartialOrder[${`A..N`}] as PartialOrder[${`(A..N)`}] given (${constraints(
                  "PartialOrder"
                )}) {
                    def partialCompare(x: ${`(A..N)`}, y: ${`(A..N)`}): Double =
                      ${binMethod("partialCompare").mkString("Array(", ", ", ")")}.find(_ != 0.0).getOrElse(0.0)
                  }
                """
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleSemigroupInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                  given Tuple${arity}Semigroup[${`A..N`}] as Semigroup[${`(A..N)`}] given (${constraints("Semigroup")}) {
                    def combine(x: ${`(A..N)`}, y: ${`(A..N)`}): ${`(A..N)`} = ${binTuple("combine")}
                  }
                """
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleSemilatticeInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                  given Tuple${arity}Semilattice[${`A..N`}] as Semilattice[${`(A..N)`}] given (${constraints(
                  "Semilattice"
                )}) {
                    def combine(x: ${`(A..N)`}, y: ${`(A..N)`}): ${`(A..N)`} = ${binTuple("combine")}
                  }
                """
            }
        )
      )
  }
}
