name := "scala2021"

Compile/mainClass := Some("scala2021.${userlogin}.task01.${ObjectName}")

scalaVersion := "2.13.5"


libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.7" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.1" % "test",
  "org.scalatestplus" %% "scalacheck-1-15" % "3.2.5.0" % "test",
  "com.beachape" %% "enumeratum" % "1.6.1",
  "org.typelevel" %% "cats-core" % "2.3.0"
)

