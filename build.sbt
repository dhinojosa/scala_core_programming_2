name := "scala_core_programming_2"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.0.5")

scalacOptions ++= Seq("-deprecation", "-feature")

EclipseKeys.withSource := true

EclipseKeys.withJavadoc := true
