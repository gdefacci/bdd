package com.github.gdefacci.bdd

import language.implicitConversions
import com.github.gdefacci.bdd.testkit.AssertionRunner

class ScenarioDescription(val title:String, desc: => Description, val filePosition:Option[FilePosition]) {
  lazy val description = desc
}
class FeatureScenario(val title: String, val run: () => List[(ScenarioDescription, List[RunEvent])])

object FeatureScenario {

  implicit def fromScenario[S, E](sc: Scenario[S, E]): FeatureScenario = {
    val desc = new ScenarioDescription(sc.title, sc.assertion.description, sc.filePosition)
    new FeatureScenario(sc.title, () => List(desc -> AssertionRunner.runEvents(sc.assertion)))
  }

  implicit def fromOutlineScenario[I, S, E](sc: OutlineScenario[I, S, E]): FeatureScenario = {
    new FeatureScenario(sc.title, () => 
      sc.scenarios.toList.map(sc => new ScenarioDescription(sc.title, sc.assertion.description, sc.filePosition) -> AssertionRunner.runEvents(sc.assertion))
    )
  }

}

class Feature(val title: String,
    val description: Option[String],
    scenarios: Seq[FeatureScenario]) {

  def this(title: String, scenarios: FeatureScenario*) = this(title, None, scenarios)
  def this(title: String, description: String, scenarios: FeatureScenario*) = this(title, Some(description), scenarios)

  def run:Seq[(String, List[(ScenarioDescription, List[RunEvent])])] = scenarios.map(s => s.title -> s.run())

} 