import sbt._
import Keys._

object Dependencies extends Build {

  val scala_2_10 = "2.10.6" 
  val scala_2_11 = "2.11.8"
  
  val allScalaVersion = Seq(scala_2_11, scala_2_10)
  
  val sbtTestInterface = "org.scala-sbt" % "test-interface" % "1.0" 
  
  val scalatest = "org.scalatest" %% "scalatest" % "2.2.6"

}
