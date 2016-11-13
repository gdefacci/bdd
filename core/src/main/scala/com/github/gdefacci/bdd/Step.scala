package com.github.gdefacci.bdd

object Flow {
  def apply[S,  E](description: Description, initialValue: () => S): Flow[S,  E] = new Flow[S,  E](description, initialValue, Nil, None)
}

class Flow[S,  E](val sourceDescription: Description, val source: () => S, val steps: Seq[FlowStep[S,  E]], val filePosition: Option[FilePosition]) {

  lazy val description:Description = steps.foldLeft(sourceDescription) { (desc, stp) => desc.add(stp.description) }
  
  private def add(conj: Conjuction, step: FlowStep[S,  E]): Flow[S,  E] =
    new Flow[S,  E](sourceDescription, source, steps :+ step.copy(step.description.prepend(conj)), filePosition)

  def But(step: FlowStep[S,  E]): Flow[S,  E] = add(Conjuction.But, step)
  def When(step: FlowStep[S,  E]): Flow[S,  E] = add(Conjuction.When, step)
  def And(step: FlowStep[S,  E]): Flow[S,  E] = add(Conjuction.And, step)
  def Then(step: FlowStep[S,  E]): Flow[S,  E] = add(Conjuction.Then, step)
  def -(step: FlowStep[S,  E]): Flow[S,  E] = add(Conjuction.-, step)

}

sealed trait FlowStep[S,  +E] {
  def description: Description
  def filePosition: Option[FilePosition]
  def copy(description: Description): FlowStep[S,  E]
  
  def fold[B](onStep:(S => S) => B, onExpectation:(S => Seq[TestResult[E]]) => B) = this match {
    case Step(_,run,_) => onStep(run)
    case Expectation(_,run,_) => onExpectation(run)

  }
  
  final override def toString = description.mkString(" ")
}

case class Step[S](val description: Description, val run: S => S, val filePosition: Option[FilePosition] = None) extends FlowStep[S,Nothing] {
  def copy(description: Description) = {
    new Step[S](description, run, filePosition)
  }
}

sealed trait TestResult[+T]

case object Ok extends TestResult[Nothing]
final case class Fail[E](err: E) extends TestResult[E]

case class Expectation[S,   E](val description: Description, val predicate: S => Seq[TestResult[E]], val filePosition: Option[FilePosition] = None) extends FlowStep[S,  E] {

  private def add(conj: Conjuction, other: Expectation[S, E]) = new Expectation[S, E](description.add(other.description.prepend(conj)), state => {
    predicate(state) ++ other.predicate(state)
  }, filePosition)

  def And(other: Expectation[S, E]) = add(Conjuction.And, other)
  def But(other: Expectation[S, E]) = add(Conjuction.But, other)
  def -(other: Expectation[S, E]) = add(Conjuction.-, other)

  def copy(description: Description) = new Expectation[S, E](description, predicate, filePosition)
}
