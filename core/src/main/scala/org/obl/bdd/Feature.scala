package org.obl.bdd

class Feature[S,E](val title:String,
  val description:Option[String],
  val scenarios:Seq[ScenarioLike[S,E]]
) {
  
  def this(title:String, scenarios:ScenarioLike[S,E]*) = this(title, None, scenarios)
  def this(title:String, description:String, scenarios:ScenarioLike[S,E]*) = this(title, Some(description), scenarios)
 
  def runnableScenarios:Seq[Scenario[S,E]] = scenarios.flatMap {
    case s @ Scenario(_,_) => s :: Nil
    case OutlineScenario(title, table, f) => table.map(f)
  }
  
}

sealed trait ScenarioLike[S,E]

case class Scenario[S,E](title:String, assertion:Assertion[S,E]) extends ScenarioLike[S,E] {
  override def toString = title+"\n\n"+assertion.toString()
}

case class OutlineScenario[I,S,E](title:String, table:Seq[I], f:I => Scenario[S,E]) extends ScenarioLike[S,E] {
  
  def scenarios:Seq[Scenario[S,E]] = table.map(f)
  
}

object OutlineScenario {
  case class Factory[I](title:String, table:Seq[I]) {
    def apply[S,E](f:I => Scenario[S,E]) = OutlineScenario[I,S,E](title, table, f)
  }
  
  
  def apply[I](title:String, table:Seq[I]) = Factory[I](title, table)
}

