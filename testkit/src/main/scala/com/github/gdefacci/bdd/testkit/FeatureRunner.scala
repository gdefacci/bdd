package com.github.gdefacci.bdd
package testkit

import language.higherKinds
import scalaz.Monad

object FeaturesRunner {
  
  def runOld(feats:Features):Seq[(Feature, String, List[RunEvent])] = {
    feats.features.flatMap { feat =>
      feat.run.flatMap {
        case (scenarioTitle, descEvents) => descEvents.map {
          case (desc, evs) => (feat, scenarioTitle, evs)
        }  
      }  
    }
  }
  
  def run(feats:Features):Seq[FeatureRun] = 
    feats.features.map { feat =>
      new FeatureRun(feat, feat.run.map {
        case (scenarioTitle, scenarios) => new ScenarioGroup(scenarioTitle, scenarios.map( p => new ScenarioRunResult(p._1, p._2)))
      })
    }
  
   
}

class FeatureRun(val feature:Feature, val scenarioGroups:Seq[ScenarioGroup]) {
  lazy val scenarioRuns:Seq[ScenarioRunResult] = scenarioGroups.flatMap(_.scenarios) 
  lazy val failedScenarios = scenarioGroups.flatMap(_.failedScenarios)

}
class ScenarioGroup(val title:String, val scenarios:Seq[ScenarioRunResult]) {
   lazy val isSuccessfull = scenarios.forall(_.isSuccessfull)
   lazy val totalTime = scenarios.map(_.totalTime).sum

   lazy val failedScenarios = scenarios.filter(!_.isSuccessfull)
  
}
class ScenarioRunResult(desc:ScenarioDescription, val events:List[RunEvent]) {
  
  lazy val title = desc.title
  lazy val description = desc.description
  lazy val filePosition:Option[FilePosition] = desc.filePosition

  lazy val isSuccessfull = events.forall {
    case _:SuccessEvent => true
    case _ => false
  }
  
  lazy val startTime = events.map(_.startTime).min
  lazy val endTime = events.map(_.endTime).max
  lazy val totalTime = events.map(_.totalTime).sum
  
}
