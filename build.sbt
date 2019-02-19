name := "dabtree"

version := "0.2"

scalaVersion := "2.12.7"

scalacOptions ++= Seq(
  "-language:higherKinds",
  "-language:existentials",
  "-language:implicitConversions",
  "-Ypartial-unification",
  "-Ywarn-value-discard",
  "-Xfatal-warnings"
)

libraryDependencies += "org.typelevel" %% "spire" % "0.16.0"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"