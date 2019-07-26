package io.circe.literal

import io.circe.{Encoder, Json, JsonNumber, JsonObject}
import java.util.UUID
import scala.compiletime.error
import scala.quoted._

object JsonLiteralMacros {
  def jsonStringContext(receiver: Expr[JsonStringContext], args: Expr[Seq[AsJson]]) given (context: QuoteContext): Expr[Json] = {
    import context.tasty._

    val stringParts: List[String] = receiver.unseal.underlyingArgument match {
      case Apply(conv, List(Apply(fun, List(Typed(Repeated(values, _), _))))) =>
        values.collect {
          case Literal(Constant(value: String)) => value
        }
    }

    val Typed(Repeated(trees, _), _) = args.unseal.underlyingArgument
    val asJsons = trees.map(_.seal.cast[AsJson])

    val withPlaceholders: Stream[(String, Expr[AsJson])] =
      Stream.continually(UUID.randomUUID().toString).distinct.filterNot(s =>
        stringParts.exists(_.contains(s))
      ).zip(asJsons)

    val jsonString = stringParts.zip(withPlaceholders.map(_._1)).foldLeft("") {
      case (acc, (part, placeholder)) =>
        val qm = "\""
        s"$acc$part$qm$placeholder$qm"
    } + stringParts.last

    def replace(s: CharSequence): Expr[Json] =
      withPlaceholders.find(_._1 == s) match {
        case None => '{Json.fromString(${s.toString.toExpr})}
        case Some(v) => '{${v._2}.value}
      }

    val facade = new org.typelevel.jawn.RawFacade[Expr[Json]] {
      final def jnull(index: Int): Expr[Json] = '{Json.Null}
      final def jfalse(index: Int): Expr[Json] = '{Json.False}
      final def jtrue(index: Int): Expr[Json] = '{Json.True}
      final def jstring(s: CharSequence, index: Int): Expr[Json] = replace(s)

      final def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Expr[Json] =
        if (decIndex < 0 && expIndex < 0) {
          '{Json.fromJsonNumber(JsonNumber.fromIntegralStringUnsafe(${s.toString.toExpr}))}
        } else {
          '{Json.fromJsonNumber(JsonNumber.fromDecimalStringUnsafe(${s.toString.toExpr}))}
        }

      final def singleContext(index: Int): org.typelevel.jawn.RawFContext[Expr[Json]] =
        new org.typelevel.jawn.RawFContext[Expr[Json]] {
          private[this] final var value: Expr[Json] = null
          final def add(s: CharSequence, index: Int): Unit = value = jstring(s, index)
          final def add(v: Expr[Json], index: Int): Unit = value = v
          final def finish(index: Int): Expr[Json] = value
          final def isObj: Boolean = false
        }

      final def arrayContext(index: Int): org.typelevel.jawn.RawFContext[Expr[Json]] =
        new org.typelevel.jawn.RawFContext[Expr[Json]] {
          private[this] final val vs = List.newBuilder[Expr[Json]]
          final def add(s: CharSequence, index: Int): Unit = vs += jstring(s, index)
          final def add(v: Expr[Json], index: Int): Unit = vs += v
          final def finish(index: Int): Expr[Json] = {
            val exprs = vs.result()
            '{Json.fromValues(${exprs.toExprOfList})}
          }
          final def isObj: Boolean = false
        }

      final def objectContext(index: Int): org.typelevel.jawn.RawFContext[Expr[Json]] =
        new org.typelevel.jawn.RawFContext[Expr[Json]] {
          private[this] final var key: String = null
          private[this] final val m = List.newBuilder[(String, Expr[Json])]

          final def add(s: CharSequence, index: Int): Unit =
            if (key.eq(null)) {
              key = s.toString
            } else {
              m += ((key, jstring(s, index)))
              key = null
            }
          final def add(v: Expr[Json], index: Int): Unit = {
            m += ((key, v))
            key = null
          }
          final def finish(index: Int): Expr[Json] = {
            val exprs = m.result().map {
              case (k, v) => '{(${k.toExpr}, $v)}
            }
            '{Json.fromFields(${exprs.toExprOfList})}
          }
          final def isObj: Boolean = true
        }
    }

    org.typelevel.jawn.Parser.parseUnsafe(jsonString)(facade)
  }
}
