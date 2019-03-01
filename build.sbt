scalaVersion in ThisBuild := "2.11.12"

lazy val commonSettings = Seq(
  version := "0.4.0",
  test in assembly := {},
  scalacOptions ++= compilerOpts
)

lazy val dabtreeCommon = (project in file("dabtree-common")).
  settings(commonSettings: _*).
  settings(
    name := "dabtree-common",
    crossScalaVersions := Seq("2.11.12", "2.12.8"),
    libraryDependencies ++= Seq(Scalactic, Scalatest, Scalacheck)
  )


// Spark implementation does not include dependency on Spire
lazy val dabtreeSpark = (project in file("dabtree-spark")).
  settings(commonSettings: _*).
  settings(
    name := "dabtree-spark",
    crossScalaVersions := Seq("2.11.12", "2.12.8"),
    mainClass in assembly := Some("com.github.robfitzgerald.dabtree.spark.example.CombinatorialSearchTrialRunner"),
    libraryDependencies ++= Seq(Decline, Cats, Spark, Scalactic, Scalatest, Scalacheck),
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case _ => MergeStrategy.first
    }
  ).
  dependsOn(dabtreeCommon)


// Cats Spire implementation does not include dependency on Spark
lazy val dabtreeCatsSpire = (project in file("dabtree-cats-spire")).
  settings(commonSettings: _*).
  settings(
    name := "dabtree-cats-spire",
    crossScalaVersions := Seq("2.11.12"),
    mainClass in assembly := Some("com.github.robfitzgerald.dabtree.example.LocalCombinatorialSearchTrialApp"),
    libraryDependencies ++= Seq(Spire, Cats, Decline, Scalactic, Scalatest, Scalacheck),
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case _ => MergeStrategy.first
    }
  ).
  dependsOn(dabtreeCommon)


lazy val compilerOpts = Seq(
  "-language:higherKinds",
  "-language:existentials",
  "-language:implicitConversions",
  "-Ypartial-unification",
  "-Ywarn-value-discard",
  "-Xfatal-warnings"
)


// numerical library
val Spire = "org.typelevel" %% "spire" % "0.14.1"
// functional abstractions
val Cats = "org.typelevel" %% "cats-core" % "1.6.0"
//val CatsEffect = "org.typelevel" %% "cats-effect" % "1.2.0"
// command-line parsing
val Decline = "com.monovore" %% "decline" % "0.5.0"
// distributed dabtree context
val Spark = "org.apache.spark" %% "spark-core" % "2.3.1" % "provided"

// pure FP typed Spark
//val framelessVersion = "0.8.0" // for Spark 2.4.0
//val framelessVersion = "0.4.1"   // for Spark 2.2.0
//val FramelessDataset = "org.typelevel" %% "frameless-dataset" % framelessVersion
////  "org.typelevel" %% "frameless-ml"      % framelessVersion,
//val FramelessCats = "org.typelevel" %% "frameless-cats"    % framelessVersion

// testing
val Scalactic = "org.scalactic" %% "scalactic" % "3.0.4"
val Scalatest = "org.scalatest" %% "scalatest" % "3.0.4" % "test"
val Scalacheck = "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
