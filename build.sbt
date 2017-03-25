import sbt.Keys._

lazy val scala_stemmer = (project in file(".")).settings(
  name := "scala-stemmer",
  version := "1.0",
  scalaVersion := "2.12.1",
  organization := "com.mizhi.nlp.stemmers.huskpaice",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "org.mockito" % "mockito-core" % "2.7.19"
  ),
  parallelExecution in Test := true,
  initialCommands in console := "import com.mizhi.nlp.stemmers.huskpaice._"
)
