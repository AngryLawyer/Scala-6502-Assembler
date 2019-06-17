ThisBuild / scalaVersion := "2.12.7"
ThisBuild / organization := "com.angrylawyer"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"
