ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "4tyTwo"
ThisBuild / organizationName := "aoc"

lazy val root = (project in file("."))
  .settings(
    name := "AOC-2022",
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
