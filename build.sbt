name := "rmchallenge"

organization := "sergeybalanin"

sbtVersion := "0.13.8"

version := "1.0.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "2.4.17")

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

fork in Test := true
