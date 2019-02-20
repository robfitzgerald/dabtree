name := "dabtree"

version := "0.2.1"

scalaVersion := "2.12.7"

mainClass in (Compile, run) := Some("com.github.robfitzgerald.dabtree.example.CombinatorialSearchTrialRunner")

scalacOptions ++= Seq(
  "-language:higherKinds",
  "-language:existentials",
  "-language:implicitConversions",
  "-Ypartial-unification",
  "-Ywarn-value-discard",
  "-Xfatal-warnings"
)

// numerical library
libraryDependencies += "org.typelevel" %% "spire" % "0.16.0"
// functional abstractions
libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"
// command-line parsing
libraryDependencies += "com.monovore" %% "decline" % "0.5.0"

// testing
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"