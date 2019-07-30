package io.circe

import given cats.syntax.eq._
import io.circe.tests.PrinterSuite

object Spaces2PrinterSuite extends PrinterSuite(Printer.spaces2, jawn.`package`) with Spaces2PrinterExample
object Spaces4PrinterSuite extends PrinterSuite(Printer.spaces4, jawn.`package`)
object NoSpacesPrinterSuite extends PrinterSuite(Printer.noSpaces, jawn.`package`)
object UnicodeEscapePrinterSuite extends PrinterSuite(Printer.noSpaces.copy(escapeNonAscii = true), jawn.`package`) {
  import given io.circe.syntax._
  test("Printing object should unicode-escape all non-ASCII chars") {
    val actual = Json.obj("0 ℃" := "32 ℉").pretty(printer)
    val expected = "{\"0 \\u2103\":\"32 \\u2109\"}"
    assert(actual === expected)
  }
}

object Spaces2PrinterWithWriterReuseSuite
    extends PrinterSuite(
      Printer.spaces2.copy(reuseWriters = true),
      jawn.`package`
    )

object Spaces4PrinterWithWriterReuseSuite
    extends PrinterSuite(
      Printer.spaces4.copy(reuseWriters = true),
      jawn.`package`
    )

object NoSpacesPrinterWithWriterReuseSuite
    extends PrinterSuite(
      Printer.noSpaces.copy(reuseWriters = true),
      jawn.`package`
    )

object UnicodeEscapePrinterWithWriterReuseSuite
    extends PrinterSuite(
      Printer.noSpaces.copy(reuseWriters = true, escapeNonAscii = true),
      jawn.`package`
    )

object Spaces2SortKeysPrinterSuite extends PrinterSuite(Printer.spaces2SortKeys, jawn.`package`) with SortedKeysSuite
object Spaces4SortKeysPrinterSuite extends PrinterSuite(Printer.spaces4SortKeys, jawn.`package`) with SortedKeysSuite
object NoSpacesSortKeysPrinterSuite extends PrinterSuite(Printer.noSpacesSortKeys, jawn.`package`) with SortedKeysSuite
object CustomIndentWithSortKeysPrinterSuite
    extends PrinterSuite(Printer.indented("   ").withSortedKeys, jawn.`package`)
    with SortedKeysSuite
