
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"
name := "boxify"

val catsVersion = "2.8.0"
val catsEffectVersion = "3.3.14"

lazy val root = (project in file("."))
  .aggregate(boxify)

lazy val boxify = (project in file("modules/boxify"))
  .settings(
    libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion,
    libraryDependencies += "org.typelevel" %% "cats-effect" % catsEffectVersion,
    scalacOptions ++= Seq("-source:future")
  )
