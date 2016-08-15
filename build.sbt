organization in ThisBuild :="org.obl.bdd"
version in ThisBuild :="0.1.0"

scalaVersion in ThisBuild := "2.11.8"

javaOptions ++= Seq("-Xss2048K", "-Xmx2g")

lazy val runFeatures = inputKey[Unit]("run a sample feture")

lazy val core = Project(
  "core",
  file("core"),
  settings = Seq(
    libraryDependencies <+= (scalaVersion)(sv => "org.scala-lang" % "scala-compiler" % sv),
    libraryDependencies += scalatest % "test"
  )
)

lazy val testkit = Project(
  "testkit",
  file("testkit"),
  settings = Seq(
    runFeatures := {
        (runMain in Test).fullInput(" org.obl.bdd.samples.TestRunMain").evaluated
    },
    libraryDependencies += scalatest % "test"
  )
).dependsOn(core)


// lazy val bddTestInterface = Project(
//   "bdd-sbt-test-interface",
//   file("bdd-test-interface"),
//   settings = Seq(
//     libraryDependencies += sbtTestInterface % "provided"
//   )
// ).dependsOn(core)

lazy val root = (project in file(".")).
  aggregate(core, testkit).
  settings(
    publishArtifact := false
  )
  


