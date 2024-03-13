ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.4"

lazy val root = (project in file("."))
  .settings(
    name := "Chip8",
    idePackagePrefix := Some("com.goosemagnet.chip8")
  )
