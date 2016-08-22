package org.obl.bdd.runner

import org.obl.bdd._
import org.scalajs.testinterface.TestUtils
import sbt.testing.{Task => BaseTask, _}

import scala.language.existentials
import scala.util.Try

final class Task(task: TaskDef, cl: ClassLoader) extends BaseTask {
  implicit val ec = scala.concurrent.ExecutionContext.global

  private lazy val sbtEvents = new SbtEvents(task)
  
  def tags(): Array[String] = Array.empty

  def taskDef(): TaskDef = task

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[BaseTask] = {

    loadFeaturesClass(task.fullyQualifiedName(), cl).fold(()) { featureHolder =>
      loggers.foreach { log =>
        log.info("\n")
        log.info(Console.GREEN + task.fullyQualifiedName() + Console.RESET)
        log.info("\n")
      }
      featureHolder.features.map { feature =>
        val featRunEves = new FeatureRunner(DefaultScenarioRunner).run(feature)
        val tinfos = new TestInfos(featRunEves)
        tinfos.scenarioOutcomes.foreach {
          case (feat, outcomes) =>
            outcomes.foreach {
              case outcome@SuccessfullScenario(scen, elapsed) =>
                
                loggers.foreach(_.info(Console.GREEN + s"  Scenario : ${scen.title}" + Console.RESET))
                eventHandler.handle(sbtEvents.success(outcome, elapsed))
                
              case outcome@FailedScenario(scen, errors) =>
                
                errors.foreach { err =>
                  loggers.foreach { log =>
                    val errTitle = s"  Failed scenario : ${scen.title}"
                    val sepCount: Int = errTitle.length + 4
                    val sep = "-" * sepCount
                    log.error(sep)
                    log.error(Console.RED + errTitle + Console.RESET)
                    log.error(sep)
                    log.error(s"${scen.filePosition.map(ln => s"($ln)\n").getOrElse("\n")}")
                    log.error(scen.assertion.toString())
                    log.error("")
                    log.error(ConsoleEventDescriptions.errorDescription(err))
                    log.error("")
                  }
                  eventHandler.handle(sbtEvents.error(err))
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
