package com.github.gdefacci.bdd

import language.higherKinds
import com.github.gdefacci.bdd.testkit.AssertionRunner
import scalaz.Monad

import language.higherKinds
import language.implicitConversions

class ScenarioDescription(val title:String, desc: => Description, val filePosition:Option[FilePosition]) {
  lazy val description = desc
}
class FeatureScenario(val title: String, val run: () => List[(ScenarioDescription, List[RunEvent])])

object FeatureScenario {

  implicit def fromScenario[S, M[_], E](sc: Scenario[S, M, E])(implicit monad: Monad[M]): FeatureScenario = {
    val desc = new ScenarioDescription(sc.title, sc.assertion.description, sc.filePosition)
    new FeatureScenario(sc.title, () => List(desc -> new AssertionRunner[M].runEvents(sc.assertion)))
  }

  implicit def fromOutlineScenario[I, S, M[_], E](sc: OutlineScenario[I, S, M, E])(implicit monad: Monad[M]): FeatureScenario = {
    val runner = new AssertionRunner[M]
    new FeatureScenario(sc.title, () => 
      sc.scenarios.toList.map(sc => new ScenarioDescription(sc.title, sc.assertion.description, sc.filePosition) -> runner.runEvents(sc.assertion))
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