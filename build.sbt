ThisBuild / scalaVersion := "2.12.7"
ThisBuild / organization := "com.angrylawyer"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases" at "http://oss.sonatype.org/content/repositories/releases"
)

lazy val assembler = (project in file("."))
  .settings(
    name := "Scala 6502 Assembler",
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    libraryDependencies += "org.scala-graph" %% "graph-core" % "1.13.0",
    libraryDependencies += "org.scala-graph" %% "graph-constrained" % "1.13.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
  )
