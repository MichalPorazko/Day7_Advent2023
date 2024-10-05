ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "Day7_Advent2023"
  )

libraryDependencies += "org.scalameta" %% "munit" % "1.0.1" % Test
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.10.7"


