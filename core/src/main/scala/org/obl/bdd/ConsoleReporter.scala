package org.obl.bdd

import java.io.PrintWriter

class ConsoleReporter(writer: PrintWriter, featureRunner: FeatureRunner) {

  private def subjectDescription[S, E](subj: EventSubject[S, E]) = subj match {
    case SourceSubject(i) => s"""The step "$i""""
    case StepSubject(i) => s"""The step "$i""""
    case ExpectationSubject(i) => s"""The expectation "$i""""
  }

  private def errordescription[S, E](err: ErrorEvent[S, E]) = {
    err match {
      case ev @ ExceptionEvent(subj, err) =>
        s"${subjectDescription(subj)}${ExceptionEvent.getInput(ev).map(i => s" with state $i ").getOrElse("")}caused error ${err.getMessage}"
      case ExpectationFailure(subj, input, err, _) =>
        s"Error: $err\n${subjectDescription(subj)} failed with state $input"
    }
  }

  private val lineSep = "-" * 120

  def report[S, E](feature: Feature[S, E]) = {
    val testInfos: TestInfos[S, E] = new TestInfos(featureRunner.run(feature))
    testInfos.scenarioOutcomes.map {
      case (feature, scenarios) =>

        val title = s"Feature  : ${feature.title}"
        writer.println("=" * title.length)
        writer.println(title)
        feature.description.foreach { desc =>
          writer.println(desc)
        }
        writer.println("=" * title.length + "\n")

        scenarios.map {
          case SuccessfullScenario(scenario, elapsed) =>
            writer.println(s"  - ${scenario.title}  completed  sucessfully ($elapsed millis)")
          case FailedScenario(scenario, errs) =>
            writer.println(lineSep)
            writer.println(s"Error !!!\n")
            writer.println(s"${scenario.toString}\n")
            errs.foreach { err =>
              writer.println(errordescription(err))
            }
            writer.println(lineSep)
        }
    }
    writer.flush()
  }

  def report(infos: Seq[Feature[_, _]]): Unit =
    infos.foreach(report(_))

}