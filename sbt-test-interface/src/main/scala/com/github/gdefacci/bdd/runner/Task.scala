package com.github.gdefacci.bdd
package runner

import testkit._
import org.scalajs.testinterface.TestUtils
import sbt.testing.{ Task => BaseTask, _ }

import scala.language.existentials
import scala.util.Try
import scala.util.Success
import scala.util.Failure

final class Task(task: TaskDef, cl: ClassLoader) extends BaseTask {
  implicit val ec = scala.concurrent.ExecutionContext.global

  private lazy val sbtEvents = new SbtEvents(task)

  def tags(): Array[String] = Array.empty

  def taskDef(): TaskDef = task

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[BaseTask] = {

    loadFeaturesClass(task.fullyQualifiedName(), cl).fold(()) { featureHolder =>
      loggers.foreach { log =>
        log.info("\n" + Console.GREEN + task.fullyQualifiedName() + Console.RESET + "\n")
      }
      val featRunEves = Try(FeaturesRunner.run(featureHolder))
      featRunEves match {
        case Failure(err) => 
          loggers.foreach(_.error(Console.RED + s"  Error : ${err.getMessage}\n${err.getStackTrace.mkString(", ")}" + Console.RESET))
          eventHandler.handle(sbtEvents.error(err))
        case Success(featRunEves) =>
          val tinfos = new TestInfos(featRunEves)
          tinfos.results.foreach { fr =>
            fr.scenarioGroups.foreach { sr =>
              if (sr.isSuccessfull) {
                loggers.foreach(_.info(Console.GREEN + s"  Scenario : ${sr.title}" + Console.RESET))
                sr.scenarios.foreach { sc =>
                  eventHandler.handle(sbtEvents.success(sc.title, sc.totalTime))
                }
              } else {
                sr.scenarios.foreach { sc =>
                  if (sc.isSuccessfull) {
                    loggers.foreach(_.info(Console.GREEN + s"  Scenario : ${sc.title}" + Console.RESET))
                    eventHandler.handle(sbtEvents.success(sc.title, sc.totalTime))
                  } else {
                    loggers.foreach { log =>
                      log.error(Descriptions.failedScenario(sc))
                    }
                    sc.events.foreach {
                      case e: ErrorEvent => eventHandler.handle(sbtEvents.error(e))
                      case _ => ()
                    }
                  }
                }
              }
            }

          }
      }

    }
    Array.empty
  }

  private def loadFeaturesClass(name: String, loader: ClassLoader): Option[Features] = {
    Try(TestUtils.loadModule(name, loader)).toOption.collect {
      case ref: Features => ref
    }
  }

}
