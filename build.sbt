ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "Day7_Advent2023"
  )

libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M11" % Test
libraryDependencies += "dev.zio" %% "zio" % "2.0.22" // Check for the latest version
libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0" // Check for the latest version


