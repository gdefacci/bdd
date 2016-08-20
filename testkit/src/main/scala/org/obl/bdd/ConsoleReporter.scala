package org.obl.bdd

import java.io.PrintWriter

class ConsoleReporter(writer: PrintWriter, featureRunner: FeatureRunner) {

  private def subjectDescription[S, E](subj: EventSubject[S, E]) = subj match {
    case SourceSubject(i) => s"""The step "$i""""
    case StepSubject(i) => s"""The step "$i""""
    case ExpectationSubject(i) => s"""The expectation "$i""""
  }

  private def errorDescription[S, E](err: ErrorEvent[S, E]) = {
    err match {
      case ev @ ExceptionEvent(subj, err) =>
        s"${subjectDescription(subj)}${ExceptionEvent.getInput(ev).map(i => s" with state $i ").getOrElse("")}caused error ${err.getMessage}"
      case ExpectationFailure(subj, input, err, _) =>
        s"Error: $err\n${subjectDescription(subj)} failed with state $input"
    }
  }

  private val lineSep = "-" * 120

  private def report[S, E](scenarioOutcomes: Seq[(Feature[S, E], Seq[ScenarioOutcome[S, E]])]) = {
    scenarioOutcomes.foreach {
      case (feature, scenarios) =>

        val title = s"Feature  : ${feature.title}"
        writer.println("=" * title.length)
        writer.println(title)
        feature.description.foreach { desc =>
          writer.println(desc)
        }
        writer.println("=" * title.length + "\n")

        scenarios.foreach {
          case SuccessfullScenario(scenario, elapsed) =>
            writer.println(s"  - ${scenario.title}  completed  sucessfully ($elapsed millis)")
          case FailedScenario(scenario, errs) =>
            writer.println(lineSep)
            writer.println(s"Error !!!\n")
            writer.println(s"${scenario.toString}\n")
            errs.foreach { err =>
              writer.println(errorDescription(err))
            }
            writer.println(lineSep)
        }
    }
    writer.flush()
  }
  
  private case class TestAcc(successScenarios:Int, failedScenarios:Int, completedSteps:Int, completedExpectations:Int, errorsNumber:Int, start:Long, end:Long) {
    def add(ti:TestInfos[_, _]):TestAcc = {
      copy(
        successScenarios = successScenarios + ti.successfullScenarios.length,
        failedScenarios = failedScenarios + ti.failedScenarios.length,
        completedSteps = completedSteps + ti.completedSteps.length,
        completedExpectations = completedExpectations + ti.successfullExpectations.length,
        errorsNumber = errorsNumber + ti.errors.length,
        start = if (start == 0) ti.startTime else start,
        end = if (end < ti.endTime) ti.endTime else end
      )
    }
  }
  
  private def reportTotals(tot:TestAcc) = {
    println(s"""
${if (tot.failedScenarios >0) "Test completed with errors !!!" else "Test completed successfully"}

total: ${tot.end-tot.start} ms, Successfull scenarios: ${tot.successScenarios}, Failed scenarios: ${tot.failedScenarios}, Steps: ${tot.completedSteps}, Expectations: ${tot.completedExpectations}

""")
  }
  
  def runFeatures(infos: Seq[Feature[_, _]]): Unit = {
    def runFeature[S, E](testAcc:TestAcc, feature: Feature[S, E]):TestAcc = {
      val testRes = new TestInfos(featureRunner.run(feature))
      
      report(testRes.scenarioOutcomes)
      testAcc.add(testRes)
    }
    val z = TestAcc(0,0,0,0,0,0,0)
    val acc = infos.foldLeft(z)(runFeature(_,_))
    reportTotals(acc)
  }

}