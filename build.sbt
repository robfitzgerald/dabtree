name := "banditsearch"

version := "0.1"

scalaVersion := "2.12.7"

scalacOptions ++= Seq(
  "-language:higherKinds",
  "-language:existentials"
)

libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"