name := "samples"
organization := "com.github.gdefacci.bdd"
version := "0.2.0-SNAPSHOT"

scalaVersion := "2.11.8"

val bddVersion = "0.2.0-SNAPSHOT"

libraryDependencies += "com.github.gdefacci.bdd" %% "sbt-test-interface" % bddVersion % "test"
libraryDependencies += "com.github.gdefacci.bdd" %% "core" % bddVersion % "test"
libraryDependencies += "com.github.gdefacci.bdd" %% "testkit" % bddVersion % "test"

testFrameworks += new TestFramework("com.github.gdefacci.bdd.runner.Framework")
