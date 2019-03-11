import sbt._

object CompilerPlugin {

  val paradise = compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
  val kindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")
}
