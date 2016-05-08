import sbt._
import sbt.Keys._
import sbtassembly.AssemblyKeys._

object Builds extends Build {

  val scalaV = "2.11.8"
  val NAME = "mongo_record"

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
        "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
        "org.mongodb" % "casbah_2.11" % "2.8.0"
      )
    )
    .dependsOn(macros)

  lazy val macros = Project(id = "macros",
    base = file("macros"))
    .settings(
      libraryDependencies := Seq(
        "org.scala-lang" % "scala-reflect" % scalaV,
        "org.scala-lang" % "scala-compiler" % scalaV,
        "org.reflections" % "reflections" % "0.9.9-RC1",
        "org.mongodb" % "casbah_2.11" % "2.8.0"
      )
    )

}