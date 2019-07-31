scalaVersion := "0.18.0-bin-20190729-0b52037-NIGHTLY"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % "0.12.0-M4",
  "io.circe" %% "circe-jawn" % "0.12.0-M4",
  // Note that the JSON interpolator won't actually work on Dotty!
  "io.circe" %% "circe-literal" % "0.12.0-M4",
  "org.typelevel" %% "cats-core" % "2.0.0-M4"
).map(_.withDottyCompat(scalaVersion.value))

