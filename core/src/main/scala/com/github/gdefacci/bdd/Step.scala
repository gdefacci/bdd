package com.github.gdefacci.bdd

import language.higherKinds

object Flow {
  def apply[S, M[_], E](description: Description, initialValue: () => S): Flow[S, M, E] = new Flow[S, M, E](description, initialValue, Nil, None)
}

class Flow[S, M[_], E](val sourceDescription: Description, val source: () => S, val steps: Seq[FlowStep[S, M, E]], val filePosition: Option[FilePosition]) {

  lazy val description:Description = steps.foldLeft(sourceDescription) { (desc, stp) => desc.add(stp.description) }
  
  private def add(conj: Conjuction, step: FlowStep[S, M, E]): Flow[S, M, E] =
    new Flow[S, M, E](sourceDescription, source, steps :+ step.copy(step.description.prepend(conj)), filePosition)

  def But(step: FlowStep[S, M, E]): Flow[S, M, E] = add(Conjuction.But, step)
  def When(step: FlowStep[S, M, E]): Flow[S, M, E] = add(Conjuction.When, step)
  def And(step: FlowStep[S, M, E]): Flow[S, M, E] = add(Conjuction.And, step)
  def Then(step: FlowStep[S, M, E]): Flow[S, M, E] = add(Conjuction.Then, step)
  def -(step: FlowStep[S, M, E]): Flow[S, M, E] = add(Conjuction.-, step)

}

sealed trait FlowStep[S, M[_], +E] {
  def description: Description
  def filePosition: Option[FilePosition]
  def copy(description: Description): FlowStep[S, M, E]
  
  def fold[B](onStep:(S => M[S]) => B, onExpectation:(M[S] => Seq[TestResult[E]]) => B) = this match {
    case Step(_,run,_) => onStep(run)
    case Expectation(_,run,_) => onExpectation(run)

  }
}

case class Step[S, M[_]](val description: Description, val run: S => M[S], val filePosition: Option[FilePosition] = None) extends FlowStep[S,M,Nothing] {
  def copy(description: Description) = {
    new Step[S,M](description, run, filePosition)
  }
}

sealed trait TestResult[+T]

case object Ok extends TestResult[Nothing]
final case class Fail[E](err: E) extends TestResult[E]

case class Expectation[S,  M[_], E](val description: Description, val predicate: M[S] => Seq[TestResult[E]], val filePosition: Option[FilePosition] = None) extends FlowStep[S, M, E] {

  private def add(conj: Conjuction, other: Expectation[S,M, E]) = new Expectation[S,M, E](description.add(other.description.prepend(conj)), state => {
    predicate(state) ++ other.predicate(state)
  }, filePosition)

  def And(other: Expectation[S,M, E]) = add(Conjuction.And, other)
  def But(other: Expectation[S,M, E]) = add(Conjuction.But, other)
  def -(other: Expectation[S,M, E]) = add(Conjuction.-, other)

  def copy(description: Description) = new Expectation[S,M, E](description, predicate, filePosition)

  override def toString = description.mkString("")
}
