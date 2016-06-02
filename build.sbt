name := "mongo_record"
organization := "dpblh"
version := "0.0.1"

scalaVersion in ThisBuild := "2.11.8"
run <<= run in Compile in core

javacOptions ++= Seq("-encoding", "UTF-8")

lazy val buildSettings = Defaults.coreDefaultSettings ++ Seq(
  version := "0.0.1",
  organization := "dpblh",
  scalaVersion := scalaVersion.value,
  name := name.value
)

lazy val macros = (project in file("macros")).settings(
  libraryDependencies := Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.mongodb" % "casbah_2.11" % "2.8.0"
  )
)

lazy val core = (project in file("core")).settings(
  libraryDependencies := Seq(
    "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
    "org.mongodb" % "casbah_2.11" % "2.8.0" % "test"
  )) dependsOn macros

lazy val migration = (project in file("migration")).settings(
  libraryDependencies := Seq(
    "org.reflections" % "reflections" % "0.9.9-RC1",
    "org.slf4j" % "slf4j-api" % "1.7.7",
    "org.mongodb" % "casbah_2.11" % "2.8.0",
    "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
  ))

lazy val main = (project in file(".")).settings(buildSettings)
    .aggregate(core, macros, migration)