organization in ThisBuild := "io.circe"

val scalaCheckDependencies = Seq(
  "org.scalacheck" %% "scalacheck" % "1.14.0"
)

val disciplineDependencies = Seq(
  "org.typelevel" %% "discipline-core" % "0.12.0-M3"
)

val testDependencies = Seq(
  "io.monix" %% "minitest" % "2.5.0",
  "io.monix" %% "minitest-laws" % "2.5.0"
)

val baseSettings = Seq(
  scalaVersion := "0.17.0-bin-20190711-e2130b9-NIGHTLY",
  scalacOptions ++= Seq("-Ykind-projector"),
  libraryDependencies ++= testDependencies.map(_ % Test).map(_.withDottyCompat(scalaVersion.value)),
  testFrameworks += new TestFramework("minitest.runner.Framework")
)

val lawsSettings = Seq(
  libraryDependencies ++= disciplineDependencies.map(_.withDottyCompat(scalaVersion.value))
)

val root =
  project
    .in(file("."))
    .settings(baseSettings)
    .aggregate(catsKernel, catsKernelLaws, catsCore, catsLaws, catsTests, circeNumbersTesting, circeNumbers, circeCore)
    .dependsOn(catsLaws, circeCore)

lazy val catsKernel = project
  .in(file("dotty-cats/kernel"))
  .settings(baseSettings)
  .settings(sourceGenerators in Compile += (sourceManaged in Compile).map(CatsKernelBoilerplate.gen).taskValue)

lazy val catsKernelLaws = project
  .in(file("dotty-cats/kernel-laws"))
  .settings(baseSettings)
  .settings(lawsSettings)
  .dependsOn(catsKernel)

lazy val catsCore = project
  .in(file("dotty-cats/core"))
  .settings(baseSettings)
  .settings(sourceGenerators in Compile += (sourceManaged in Compile).map(CatsBoilerplate.gen).taskValue)
  .dependsOn(catsKernel)

lazy val catsLaws = project
  .in(file("dotty-cats/laws"))
  .settings(baseSettings)
  .settings(lawsSettings)
  .dependsOn(catsCore, catsKernelLaws)

lazy val catsTests = project
  .in(file("dotty-cats/tests"))
  .settings(baseSettings)
  .settings(
    libraryDependencies ++= testDependencies.map(_.withDottyCompat(scalaVersion.value))
  )
  .dependsOn(catsLaws, catsKernelLaws)

lazy val circeNumbersTesting = project
  .in(file("dotty-circe/numbers-testing"))
  .settings(baseSettings)
  .settings(libraryDependencies ++= scalaCheckDependencies.map(_.withDottyCompat(scalaVersion.value)))

lazy val circeNumbers = project
  .in(file("dotty-circe/numbers"))
  .settings(baseSettings)
  .dependsOn(circeNumbersTesting % Test)

lazy val circeCore = project
  .in(file("dotty-circe/core"))
  .settings(baseSettings)
  .settings(sourceGenerators in Compile += (sourceManaged in Compile).map(CirceBoilerplate.gen).taskValue)
  .dependsOn(circeNumbers, catsCore)
