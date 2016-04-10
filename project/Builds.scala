import sbt._
import sbt.Keys._
import sbtassembly.AssemblyKeys._

object Builds extends Build {

  val sprayVersion = "1.3.2"
  val akkaVersion = "2.3.9"
  val scalaV = "2.11.7"
  val NAME = "kassa"

  javacOptions ++= Seq("-encoding", "UTF-8")

  lazy val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    version := "0.0.1",
    organization := "TEKO",
    scalaVersion := scalaV,
    name := NAME//,
//    libraryDependencies := Seq(
//      "org.scala-lang" % "scala-reflect" % scalaV,
//      "org.scala-lang" % "scala-compiler" % scalaV
//    )
  )

  lazy val main = Project(
    NAME,
    file("."),
    settings = buildSettings
  )
    .aggregate(core, macros)

  lazy val core = Project(id = "core",
    base = file("core"))
    .dependsOn(macros)

  lazy val macros = Project(id = "macros",
    base = file("macros"))
    .settings(
      libraryDependencies := Seq(
        "org.scala-lang" % "scala-reflect" % scalaV,
        "org.scala-lang" % "scala-compiler" % scalaV
      )
    )

}