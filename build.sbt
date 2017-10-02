enablePlugins(ScalaJSPlugin)


name := "scala-tak"
version := "1.0"
scalaVersion := "2.12.3"

scalaJSUseMainModuleInitializer := true

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies ++= Seq(
  "com.github.julien-truffaut" %%%  "monocle-core"  % "1.4.0",
  "com.github.julien-truffaut" %%%  "monocle-macro" % "1.4.0",
  "com.github.julien-truffaut" %%%  "monocle-law"   % "1.4.0" % "test"
)
