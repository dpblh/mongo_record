import sbt._
import sbt.Keys._
import sbtassembly.AssemblyKeys._

object Builds extends Build {

  val scalaV = "2.11.7"
  val NAME = "kassa"

  javacOptions ++= Seq("-encoding", "UTF-8")

  lazy val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    version := "0.0.1",
    organization := "TEKO",
    scalaVersion := scalaV,
    name := NAME,
    libraryDependencies := Seq(

    )
  )

  lazy val main = Project(
    NAME,
    file("."),
    settings = buildSettings
  )
    .aggregate(core, macros)

  lazy val core = Project(id = "core",
    base = file("core"))
    .settings(
      libraryDependencies := Seq(
        "org.json4s" %% "json4s-jackson" % "3.2.11",
        "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
      )
    )
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