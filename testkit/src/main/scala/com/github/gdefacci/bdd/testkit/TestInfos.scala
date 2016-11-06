package com.github.gdefacci.bdd
package testkit

import language.higherKinds
import com.github.gdefacci.bdd.Feature

//Seq[FeatureRun]
class TestInfos(val results: Seq[FeatureRun]) {

  lazy val resultEvents: Seq[RunEvent] = results.flatMap { fr =>
    fr.scenarioGroups.flatMap { sg =>
      sg.scenarios.flatMap {
        sc => sc.events
      }
    }
  }

  lazy val (startTime: Long, endTime: Long) = results.flatMap(_.scenarioGroups.flatMap(_.scenarios)).foldLeft(0l -> 0l) { (acc, i) =>
    val (mn, mx) = acc
    Math.min(mn, i.startTime) -> Math.max(mx, i.endTime)
  }

  lazy val totalTime: Long = endTime - startTime

  lazy val (successfullScenarios, failedScenarios) = results.flatMap(_.scenarioRuns).partition(_.isSuccessfull)

  lazy val errors: Seq[ErrorEvent] = resultEvents.collect {
    case ev: ErrorEvent => ev
  }

  lazy val completedSteps: Seq[StepSuccess] = resultEvents.collect {
    case ev: StepSuccess => ev
  }

  lazy val successfullExpectations: Seq[ExpectationSuccess] = resultEvents.collect {
    case ev: ExpectationSuccess => ev
  }

}
