organization in ThisBuild :="com.github.gdefacci.bdd"
version in ThisBuild :="0.2.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.8"

javaOptions ++= Seq("-Xss2048K", "-Xmx2g")

lazy val runFeatures = inputKey[Unit]("run a sample feture")

lazy val scalaMacros: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "macro-compat" % "1.1.1",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
)

lazy val commonBuildSettings = Seq(
  scalaVersion := scala_2_11,
  crossScalaVersions := allScalaVersion,
  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += Resolver.sonatypeRepo("releases"),
  scalacOptions ++= Seq("-deprecation"),
  publishMavenStyle := true
)

lazy val core = Project(
  "core",
  file("core"),
  settings = commonBuildSettings ++ scalaMacros ++ Seq(
    libraryDependencies += scalatest % "test"
  )
)

lazy val testkit = Project(
  "testkit",
  file("testkit"),
  settings = commonBuildSettings ++ Seq(
    libraryDependencies += scalatest % "test"
  )
).dependsOn(core)

lazy val bddSbtTestInterface = Project(
  "sbt-test-interface",
  file("sbt-test-interface"),
  settings = commonBuildSettings ++ Seq(
    libraryDependencies += sbtTestInterface % "provided",
    libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0",
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
  )
).dependsOn(core, testkit)

lazy val root = (project in file(".")).
  aggregate(core, testkit, bddSbtTestInterface).
  settings(
    commonBuildSettings ++ Seq(publishArtifact := false)
  ) 