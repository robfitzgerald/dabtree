//name := "dabtree"

//version := "0.2.1"

//scalaVersion := "2.12.7"

//mainClass in (Compile, run) := Some("com.github.robfitzgerald.dabtree.example.CombinatorialSearchTrialRunner")


lazy val commonSettings = Seq(
  name := "dabtree",
  version := "0.2.2",
//  organization := "com.example",
  scalaVersion := "2.12.7",
  test in assembly := {},
  scalacOptions ++= compilerOpts
)

lazy val app = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    mainClass in assembly := Some("com.github.robfitzgerald.dabtree.example.CombinatorialSearchTrialRunner"),
    libraryDependencies ++= Seq(
      Spire, Cats, Decline, Scalactic, Scalatest, Scalacheck
    ),
    assemblyMergeStrategy in assembly := {
      //    case "META-INF/services/org.apache.livy.LivyClientFactory" => MergeStrategy.last
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    }
    // more settings here ...
  )
//
//lazy val utils = (project in file("utils")).
//  settings(commonSettings: _*).
//  settings(
//    assemblyJarName in assembly := "utils.jar",
//    // more settings here ...
//  )

lazy val compilerOpts = Seq(
  "-language:higherKinds",
  "-language:existentials",
  "-language:implicitConversions",
  "-Ypartial-unification",
  "-Ywarn-value-discard",
  "-Xfatal-warnings"
)

//scalacOptions ++= Seq(
//  "-language:higherKinds",
//  "-language:existentials",
//  "-language:implicitConversions",
//  "-Ypartial-unification",
//  "-Ywarn-value-discard",
//  "-Xfatal-warnings"
//)

// numerical library
val Spire = "org.typelevel" %% "spire" % "0.16.0"
// functional abstractions
val Cats = "org.typelevel" %% "cats-core" % "1.6.0"
//val CatsEffect = "org.typelevel" %% "cats-effect" % "1.2.0"
// command-line parsing
val Decline = "com.monovore" %% "decline" % "0.5.0"

// testing
val Scalactic = "org.scalactic" %% "scalactic" % "3.0.4"
val Scalatest = "org.scalatest" %% "scalatest" % "3.0.4" % "test"
val Scalacheck = "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

//// numerical library
//libraryDependencies += "org.typelevel" %% "spire" % "0.16.0"
//// functional abstractions
//libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"
//// command-line parsing
//libraryDependencies += "com.monovore" %% "decline" % "0.5.0"
//
//// testing
//libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
//libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
//libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"