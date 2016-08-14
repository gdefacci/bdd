package org.obl.bdd

sealed trait ScenarioOutcome[S,E]
case class SuccessfullScenario[S,E](scenario:Scenario[S,E], elapsed:Long) extends ScenarioOutcome[S,E]
case class FailedScenario[S,E](scenario:Scenario[S,E], errors:Seq[ErrorEvent[S, E]]) extends ScenarioOutcome[S,E]

class TestInfos[S,E](featureEvents:Seq[FeatureRunEvent[S,E]]) {

  private lazy val testEvents = featureEvents.map(_.event)
  
  private lazy val startTimeEvents = featureEvents.collect {
    case FeatureRunEvent(feature, scenario, StartEvent(SourceSubject(_), time)) => (feature, scenario, time)
  }
  
  private lazy val featureAndScenarios = startTimeEvents.map( e => e._1 -> e._2 ).groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }
  
  private lazy val startTimeMap = startTimeEvents.collect {
    case (_, scenario, time) => scenario -> time
  }.toMap

  private lazy val successEndTimeMap = (featureEvents.collect {
    case FeatureRunEvent(_, scenario, SuccessEvent(subj, _, time)) => scenario -> time
  }).groupBy(_._1).map {
    case (k, v) => k -> v.map(_._2).max
  }
  
  private def duration(scenario:Scenario[S,E]) = successEndTimeMap(scenario) - startTimeMap(scenario)
  
  private lazy val errors:Map[Scenario[S, E], Seq[ErrorEvent[S, E]]] = (featureEvents.collect {
    case FeatureRunEvent(_, scenario, ev @ ExceptionEvent(subj, err)) => scenario -> ev
    case FeatureRunEvent(_, scenario, ev @ ExpectationFailure(subj, _, _, _)) => scenario -> ev
  }).groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }
  
  lazy val scenarioOutcomes:Seq[(Feature[S,E], Seq[ScenarioOutcome[S,E]])] = featureAndScenarios.map { p =>
    val (feature, scenarios) = p
    feature -> (scenarios.map { scenario =>
      errors.get(scenario) match {
        case None =>
          SuccessfullScenario(scenario, duration(scenario))
        case Some(errs) =>
          FailedScenario(scenario, errs)
      }
    })
  }.toSeq

}