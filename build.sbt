name := "scala_programming_fundamentals_2"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.0.5")

scalacOptions ++= Seq("-deprecation", "-feature")

EclipseKeys.withSource := true

EclipseKeys.withJavadoc := true
