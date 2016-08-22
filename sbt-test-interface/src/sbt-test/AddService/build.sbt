name := "add-service-demo"
organization := "org.obl.bdd"
version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.8"

val bddVersion = "0.1.0"

libraryDependencies += "org.obl.bdd" %% "sbt-test-interface" % bddVersion % "test"
libraryDependencies += "org.obl.bdd" %% "core" % bddVersion % "test"
libraryDependencies += "org.obl.bdd" %% "testkit" % bddVersion % "test"

testFrameworks += new TestFramework("org.obl.bdd.runner.Framework")
