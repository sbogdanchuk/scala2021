name := "scala2021"

Compile/mainClass := Some("scala2021.${userlogin}.task01.${ObjectName}")

scalaVersion := "2.13.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"

