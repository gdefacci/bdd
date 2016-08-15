package org.obl.bdd

import scala.util.{ Try, Success, Failure }

trait ScenarioRunner {
  def run[S, E](scenario: Scenario[S, E]): Seq[ScenarioRunEvent[S, E]]
}

case class FeatureRunEvent[S, E](feature: Feature[S, E], scenario: Scenario[S, E], event: ScenarioRunEvent[S, E])

case class FeatureRunner(scenarioRunner: ScenarioRunner) {

  def run[S, E](feature: Feature[S, E]): Seq[FeatureRunEvent[S, E]] =
    feature.runnableScenarios.map(scenario => scenario -> scenarioRunner.run(scenario)).flatMap {
      case (scenario, evs) => evs.map(ev => FeatureRunEvent[S, E](feature, scenario, ev))
    }

}

object DefaultScenarioRunner extends ScenarioRunner {

  type EventsOutCome[S, E, T] = Either[Seq[ScenarioRunEvent[S, E]], (T, Seq[ScenarioRunEvent[S, E]])]

  private def sourceEvents[S, E](source: Source[S]): EventsOutCome[S, E, S] = {
    val subj: SourceSubject[S, E] = SourceSubject(source)
    val start = StartEvent(subj, System.currentTimeMillis)
    Try(source.initialValue()) match {
      case Success(result) => Right(result -> (start :: SuccessEvent(subj, result, System.currentTimeMillis) :: Nil))
      case Failure(err) => Left(start :: SourceError(subj, System.currentTimeMillis, err) :: Nil)
    }
  }

  private def expectationEvents[S, E](expectation: Expectation[S, E], input: => S): Seq[ScenarioRunEvent[S, E]] = {
    val subj = ExpectationSubject(expectation)
    val start = StartEvent(subj, System.currentTimeMillis)
    Try(expectation.predicate(input)) match {
      case Success(outcome) =>
        val failures = outcome.collect {
          case Fail(err) => err
        }
        if (failures.isEmpty) {
          start :: SuccessEvent(subj, input, System.currentTimeMillis) :: Nil
        } else {
          start +: failures.map { failure =>
            ExpectationFailure[S, E](ExpectationSubject(expectation), input, failure, System.currentTimeMillis)
          }
        }
      case Failure(err) =>
        start :: ExpectationError(ExpectationSubject(expectation), input, System.currentTimeMillis, err) :: Nil
    }
  }

  private def stepEvents[S, E](stp: Step[S], input: => S): EventsOutCome[S, E, S] = {
    val subj = StepSubject[S, E](stp)
    val start = StartEvent(subj, System.currentTimeMillis)
    Try(stp.run(input)) match {
      case Success(result) => Right(result -> (start :: SuccessEvent(subj, result, System.currentTimeMillis) :: Nil))
      case Failure(err) => Left(start :: StepError(subj, input, System.currentTimeMillis, err) :: Nil)
    }
  }

  def run[S, E](scenario: Scenario[S, E]): Seq[ScenarioRunEvent[S, E]] = {
    val assertion = scenario.assertion
    (sourceEvents[S, E](assertion.source).right.flatMap {
      case (src, events) =>
        val z: EventsOutCome[S, E, S] = Right(src -> events)
        assertion.source.steps.foldLeft(z) { (acc, step) =>
          acc.right.flatMap {
            case (input, evs) =>
              stepEvents[S, E](step, input).fold(errs => Left(evs ++ errs), {
                case (out, evs1) => Right(out -> (evs ++ evs1))
              })
          }
        }
    }.right.map {
      case (input, events) =>
        events ++ assertion.expectations.flatMap { expectation =>
          expectationEvents[S, E](expectation, input)
        }
    }).fold(identity, identity)
  }

}