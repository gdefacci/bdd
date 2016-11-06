package org.obl.bdd

import org.obl.bdd._
import org.scalatools.testing.Logger
import org.scalatools.testing.Runner2
import org.scalatools.testing.EventHandler

// sbt.testing.SubclassFingerprint
// org.scalatools.testing.SubclassFingerprint with 
object FetureSubclass extends sbt.testing.SubclassFingerprint  {
  val isModule: Boolean = false
  
  val superclassName = "org.obl.bdd.Feature"
  
  val requireNoArgConstructor = false
}

object FetureSubclass1 extends sbt.testing.SubclassFingerprint {
  val isModule: Boolean = true
  
  val superclassName = "org.obl.bdd.Feature"
  
  val requireNoArgConstructor = true
}

object FetureSubclass2 extends sbt.testing.SubclassFingerprint {
  val isModule: Boolean = true
  
  val superclassName = "org.obl.bdd.Feature"
  
  val requireNoArgConstructor = false
}

object FetureSubclass3 extends sbt.testing.SubclassFingerprint {
  val isModule: Boolean = false
  
  val superclassName = "org.obl.bdd.Feature"
  
  val requireNoArgConstructor = true
}

// org.scalatools.testing.Framework
// sbt.testing.Framework
class BDDFramework extends sbt.testing.Framework {
  val name = "BDD"

  println("BDDFramework")
  
//  val tests = Array[org.scalatools.testing.Fingerprint](FetureSubclass, FetureSubclass1, FetureSubclass2, FetureSubclass3)
//  
//  def testRunner(testClassLoader: ClassLoader, loggers: Array[Logger]) = {
//    loggers.foreach( log => log.warn("BDDSbtTestRunner ") )
//    println("BDDSbtTestRunner")
//  
//    new BDDSbtTestRunner(testClassLoader, loggers)
//  }
  
  def fingerprints(): Array[sbt.testing.Fingerprint] = Array(FetureSubclass, FetureSubclass1, FetureSubclass2, FetureSubclass3)
  
  def runner(args: Array[String],remoteArgs: Array[String],testClassLoader: ClassLoader): sbt.testing.Runner = {
    println("BDDSbtTestRunner1")
    new BDDSbtTestRunner1(args, remoteArgs, testClassLoader)
  }

}

import sbt.testing.Task;
import sbt.testing.TaskDef;

class BDDSbtTestRunner1(val args: Array[String], val remoteArgs: Array[String],testClassLoader: ClassLoader) extends sbt.testing.Runner {

  println("args "+args.mkString(", "))
  println("remote args "+remoteArgs.mkString(", "))
  
  def tasks(taskDefs:Array[TaskDef]): Array[Task] = {
    println("BDDSbtTestRunner1 tasks")
    taskDefs.map { taskDef =>
      val testClassName = taskDef.fullyQualifiedName();
      println("TestClassName "+testClassName)
      createTask(taskDef)
    }
  }
  
  private def createTask(td:TaskDef): Task = 
    new Task() {
      lazy val tags = Array.empty[String]
      lazy val taskDef = td
      
      def execute(eventHandler:sbt.testing.EventHandler, loggers:Array[sbt.testing.Logger]) = {
        loggers.foreach (_.warn("BDDSbtTestRunner createTask :"))
        Array.empty[Task]
      }
    }
 
  def done = "my-done"
}

class BDDSbtTestRunner(testClassLoader: ClassLoader, loggers: Array[Logger]) extends Runner2 {
  
  loggers.foreach( log => log.warn("BDDSbtTestRunner ") )
  println("BDDSbtTestRunner")
  
  def run(testClassName: String, fingerprint: org.scalatools.testing.Fingerprint, eventHandler: EventHandler, testOptions: Array[String]):Unit = {
    loggers.foreach (_.warn("Feature class:" + testClassName))
    
    val featureObjectClass = Class.forName(testClassName, true, testClassLoader)
    
    val features: Seq[Feature[_, _]] = featureObjectClass.asInstanceOf[Feature[Any,Any]] :: Nil

    loggers.foreach( log => log.warn("testClassName "+testClassName) )
    
    val featureRunner = FeatureRunner(DefaultScenarioRunner)

    new ConsoleReporter(new java.io.PrintWriter(System.out), featureRunner).runFeatures(features)
  }
}
