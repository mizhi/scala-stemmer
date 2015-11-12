import sbt.Keys._

lazy val huskpaice = (project in file(".")).settings(
  name := "huskpaice",
  version := "1.0",
  scalaVersion := "2.11.7",
  organization := "com.mizhi.nlp.stemmers.huskpaice",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    "org.mockito" % "mockito-all" % "1.10.19"),
  parallelExecution in Test := true,
  initialCommands in console := "import com.mizhi.nlp.stemmers.huskpaice._"
)
