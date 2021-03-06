import sbt._
import Keys._

object BuildSettings {
  val paradiseVersion = "2.1.0"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "dpblh",
    version := "1.0.0",
    scalacOptions ++= Seq(),
    scalaVersion := "2.11.8",
    crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.10.5", "2.10.6", "2.11.0", "2.11.1", "2.11.2", "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7", "2.11.8"),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      run <<= run in Compile in mongo_record_macros
    )
  ) aggregate(mongo_record_macros, mongo_record_core, migration)

  lazy val mongo_record_macros: Project = Project(
    "mongo_record_macros",
    file("mongo_record_macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "org.mongodb" % "casbah_2.11" % "2.8.0",
        "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
      ),
      libraryDependencies ++= (
        if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
        else Nil
      )
    )
  ) dependsOn mongo_record_core

  lazy val mongo_record_core: Project = Project(
    "mongo_record_core",
    file("mongo_record_core"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "org.mongodb" % "casbah_2.11" % "2.8.0",
        "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
      )
    )
  )

  lazy val migration: Project = Project(
    "migration",
    file("migration"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(
        "org.reflections" % "reflections" % "0.9.9-RC1",
        "org.slf4j" % "slf4j-api" % "1.7.7",
        "org.mongodb" % "casbah_2.11" % "2.8.0",
        "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
      )
    )
  )

}