package org.obl.bdd.runner

import sbt.testing.{Framework => BaseFramework, _}

class Framework extends BaseFramework {
  def name: String = "bdd"

  def fingerprints(): Array[Fingerprint] =
    Array(Framework.ModuleFingerprint)

  def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): Runner =
    new Runner(args, remoteArgs, testClassLoader)

  def slaveRunner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader, send: (String) => Unit): Runner =
    runner(args, remoteArgs, testClassLoader)
}

object Framework {

  object ModuleFingerprint extends SubclassFingerprint {
    val isModule = true
    def requireNoArgConstructor(): Boolean = true
    def superclassName(): String = "org.obl.bdd.Features"
  }

}