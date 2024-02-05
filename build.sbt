ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "parse",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.scalactic" %% "scalactic" % "3.2.17",
      "org.scalatest" %% "scalatest" % "3.2.17" % Test,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % Test
    )
  )
