package com.github.gdefacci.bdd

import language.higherKinds

sealed trait ScenarioLike[S, M[_], E]

final case class Scenario[S, M[_], E](title: String, assertion: Flow[S, M, E], filePosition: Option[FilePosition]) extends ScenarioLike[S, M, E] {
  override def toString = title + "\n\n" + assertion.toString()
}

object Scenario {
  def apply[S, M[_], E](title: String, assertion: Flow[S, M, E]) =
    new Scenario(title, assertion, None)
}

final case class OutlineScenario[I, S, M[_], E](title: String, table: Seq[I], f: I => Scenario[S, M, E]) extends ScenarioLike[S, M, E] {

  def scenarios: Seq[Scenario[S, M, E]] = table.map(f)

}

object OutlineScenario {
  final case class Factory[I](title: String, table: Seq[I]) {
    def apply[S, M[_], E](f: I => Scenario[S, M, E]) = OutlineScenario[I, S, M, E](title, table, f)
  }

  def apply[I](title: String, table: Seq[I]) = Factory[I](title, table)
}

