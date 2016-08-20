package org.obl.bdd

class Feature[S, E](val title: String,
    val description: Option[String],
    val scenarios: Seq[ScenarioLike[S, E]]) {

  def this(title: String, scenarios: ScenarioLike[S, E]*) = this(title, None, scenarios)
  def this(title: String, description: String, scenarios: ScenarioLike[S, E]*) = this(title, Some(description), scenarios)

  def runnableScenarios: Seq[Scenario[S, E]] = scenarios.flatMap {
    case s @ Scenario(_, _,_) => s :: Nil
    case OutlineScenario(title, table, f) => table.map(f)
  }

}