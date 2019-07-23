package io.circe.exported

case class Exported[+T](instance: T) extends AnyVal
