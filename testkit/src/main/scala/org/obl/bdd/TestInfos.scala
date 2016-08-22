package org.obl.bdd

sealed trait ScenarioOutcome[S, E]
case class SuccessfullScenario[S, E](scenario: Scenario[S, E], elapsed: Long) extends ScenarioOutcome[S, E]
case class FailedScenario[S, E](scenario: Scenario[S, E], errors: Seq[ErrorEvent[S, E]]) extends ScenarioOutcome[S, E]

class TestInfos[S, E](featureEvents: Seq[FeatureRunEvent[S, E]]) {

  private lazy val scenarioStartTimeEvents = featureEvents.collect {
    case FeatureRunEvent(feature, scenario, ev @ StartEvent(SourceSubject(_), _)) => (feature, scenario, ev)
  }

  private lazy val startTimeMap = scenarioStartTimeEvents.collect {
    case (_, scenario, ev) => scenario -> ev.time
  }.toMap

  private lazy val successEventsMap = (featureEvents.collect {
    case FeatureRunEvent(_, scenario, ev @ SuccessEvent(_, _, _)) => scenario -> ev
  })
  
  def maxOrZero(ls:Iterable[Long]) = if (ls.isEmpty) 0 else ls.max

  private lazy val successEndTimeMap = (successEventsMap.collect {
    case (scenario, SuccessEvent(subj, _, time)) => scenario -> time
  }).groupBy(_._1).map {
    case (k, v) => k -> maxOrZero(v.map(_._2))
  }

  private def duration(scenario: Scenario[S, E]) = successEndTimeMap(scenario) - startTimeMap(scenario)

  private lazy val errorsMap: Map[Scenario[S, E], Seq[ErrorEvent[S, E]]] = (featureEvents.collect {
    case FeatureRunEvent(_, scenario, ev @ ExceptionEvent(subj, err)) => scenario -> ev
    case FeatureRunEvent(_, scenario, ev @ ExpectationFailure(subj, _, _, _)) => scenario -> ev
  }).groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }

  lazy val scenariosByFeature: Map[Feature[S, E], Seq[Scenario[S, E]]] =
    scenarioStartTimeEvents.map(e => e._1 -> e._2).groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }

  lazy val scenarioOutcomes: Seq[(Feature[S, E], Seq[ScenarioOutcome[S, E]])] = scenariosByFeature.map { p =>
    val (feature, scenarios) = p
    feature -> (scenarios.map { scenario =>
      errorsMap.get(scenario) match {
        case None =>
          SuccessfullScenario(scenario, duration(scenario))
        case Some(errs) =>
          FailedScenario(scenario, errs)
      }
    })
  }.toSeq

  lazy val errors: Seq[ErrorEvent[S, E]] = errorsMap.flatMap(_._2).toSeq

  lazy val failedScenarios: Seq[FailedScenario[S, E]] = scenarioOutcomes.flatMap {
    case (_, scenarios) => scenarios.collect {
      case s @ FailedScenario(_, _) => s
    }
  }

  lazy val successfullScenarios: Seq[SuccessfullScenario[S, E]] = scenarioOutcomes.flatMap {
    case (_, scenarios) => scenarios.collect {
      case s @ SuccessfullScenario(_, _) => s
    }
  }

  lazy val completedSteps: Seq[Step[S]] = successEventsMap.collect {
    case (_, SuccessEvent(StepSubject(step), _, _)) => step
  }

  lazy val successfullExpectations: Seq[Expectation[S, E]] = successEventsMap.collect {
    case (_, SuccessEvent(ExpectationSubject(exp), _, _)) => exp
  }

  lazy val startTime: Long = if (startTimeMap.values.isEmpty) Long.MaxValue else startTimeMap.values.min

  lazy val endTime: Long = Math.max(maxOrZero(successEndTimeMap.values), maxOrZero(errors.map(_.time)))

  lazy val totalTime: Long = endTime - startTime
}