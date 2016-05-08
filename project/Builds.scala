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
    .aggregate(core, macros, casbah_migration)

  lazy val core = Project(id = "core",
    base = file("core"))
    .settings(
      libraryDependencies := Seq(
        "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
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
        "org.slf4j" % "slf4j-api" % "1.7.7"
      )
    )

  lazy val casbah_migration = Project(id = "casbah_migration",
    base = file("casbah_migration"))
    .settings(
      libraryDependencies := Seq(
        "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
        "org.mongodb" % "casbah_2.11" % "2.8.0"
      )
    )
    .dependsOn(core, macros)

}