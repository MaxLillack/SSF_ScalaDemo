import sbt._
import Keys._

object BuildSettings {
  val scalatest = "org.scalatest" % "scalatest_2.11" % "2.1.6" % "test"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    version := "1.0.0",
    scalaVersion := "2.11.0",
    crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.11.0"),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq(),
	libraryDependencies ++= Seq(scalatest)
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      run <<= run in Compile in core)
  ) aggregate(macros, core)

  lazy val macros: Project = Project(
    "macros",
    file("macro"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _))
  )

  lazy val core: Project = Project(
    "core",
    file("core"),
    settings = buildSettings
  ) dependsOn(macros)
}