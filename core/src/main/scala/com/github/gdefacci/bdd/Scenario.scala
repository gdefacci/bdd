package com.github.gdefacci.bdd

sealed trait ScenarioLike[S, E]

final case class Scenario[S, E](title: String, assertion: Flow[S, E], filePosition: Option[FilePosition] = None) extends ScenarioLike[S, E] {
  override def toString = title + "\n\n" + assertion.toString()
}

final case class OutlineScenario[I, S, E](title: String, table: Seq[I], f: I => Scenario[S, E]) extends ScenarioLike[S, E] {

  def scenarios: Seq[Scenario[S, E]] = table.map(f)

}

object OutlineScenario {
  final case class Factory[I](title: String, table: Seq[I]) {
    def apply[S, E](f: I => Scenario[S, E]) = OutlineScenario[I, S, E](title, table, f)
  }

  def apply[I](title: String, table: Seq[I]) = Factory[I](title, table)
}

