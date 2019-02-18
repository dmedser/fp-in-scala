name := "rbp-fp-in-scala"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" %"1.5.0",
  "org.typelevel" %% "cats-laws" % "1.5.0",
  "org.typelevel" %% "cats-testkit" % "1.5.0" % Test,
  "com.comcast" %% "ip4s" % "1.0.2",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test
)

libraryDependencies ++= {
  import Dependency._

  val compile = List(
    monix
  )

  val test = List(
    scalaTest
  )

  val compilerPlugins = List(
    CompilerPlugin.paradise,
    CompilerPlugin.kindProjector
  )

  compile ++ test.map(_ % Test) ++ compilerPlugins
}