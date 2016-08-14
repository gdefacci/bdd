import sbt._
import Keys._

object Dependencies extends Build {

  val sbtTestInterface = "org.scala-sbt" % "test-interface" % "1.0" 
  
  val scalatest = "org.scalatest" %% "scalatest" % "2.2.6"

}
