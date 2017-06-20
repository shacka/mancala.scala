import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "me.tsymbala.andrii",
      scalaVersion := "2.12.2",
      version      := "0.0.1-SNAPSHOT"
    )),
    name := "Mancala",
    libraryDependencies += scalaTest % Test
  )
