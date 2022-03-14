ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "cryptoExchangeTesting"
  )


libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.2"