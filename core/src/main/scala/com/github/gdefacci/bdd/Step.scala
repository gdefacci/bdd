package com.github.gdefacci.bdd

import language.higherKinds

object Flow {
  def apply[S, M[_]](description: Description, initialValue: () => S): Flow[S, M] = new Flow[S, M](description, initialValue, Nil, None)
}

class Flow[S, M[_]](val sourceDescription: Description, val source: () => S, val steps: Seq[Step[S, M]], val filePosition: Option[FilePosition]) {

  lazy val description:Description = steps.foldLeft(sourceDescription) { (desc, stp) => desc.add(stp.description) }
  
  private def add(conj: Conjuction, step: Step[S, M]): Flow[S, M] =
    new Flow[S, M](sourceDescription, source, steps :+ step.copy(step.description.prepend(conj)), filePosition)

  private def add[E](conj: Conjuction, expect: Expectation[S, M, E]) =
    new Assertion[S, M, E](this, expect.copy(description = expect.description.prepend(conj)) :: Nil)

  def But(step: Step[S, M]): Flow[S, M] = add(Conjuction.But, step)
  def When(step: Step[S, M]): Flow[S, M] = add(Conjuction.When, step)
  def And(step: Step[S, M]): Flow[S, M] = add(Conjuction.And, step)
  def Then(step: Step[S, M]): Flow[S, M] = add(Conjuction.Then, step)
  def -(step: Step[S, M]): Flow[S, M] = add(Conjuction.-, step)

  def And[E](expect: Expectation[S, M, E]) = add(Conjuction.And, expect)
  def Then[E](expect: Expectation[S,M, E]) = add(Conjuction.Then, expect)
  def -[E](expect: Expectation[S, M, E]) = add(Conjuction.-, expect)

}

class Step[S, M[_]](val description: Description, val run: S => M[S], val filePosition: Option[FilePosition]) {
  def copy(description: Description) = {
    new Step[S,M](description, run, filePosition)
  }
}

object Step {

  def apply[S,M[_]](description: Description, step: S => M[S], filePosition: Option[FilePosition]) = {
    new Step[S,M](description, step, filePosition)
  }
  
   def apply[S,M[_]](description: Description, step: S => M[S]) = {
    new Step[S,M](description, step, None)
  }

}

case class AssertionDescription(source: Description, expectations: Description) {

  lazy val description = source.add(expectations) 
  
  override def toString = source.mkString("\n") + "\n" + expectations.mkString("\n")
}

sealed trait TestResult[+T]

case object Ok extends TestResult[Nothing]
final case class Fail[E](err: E) extends TestResult[E]

class Assertion[S, M[_], E](val flow: Flow[S, M], val expectations: Seq[Expectation[S,M, E]]) {

  private def add(conj: Conjuction, expect: Expectation[S, M,E]): Assertion[S, M, E] =
    new Assertion[S, M, E](flow, expectations :+ expect.copy(description = expect.description.prepend(conj)))

  def And(expect: Expectation[S, M,E]): Assertion[S, M, E] = add(Conjuction.And, expect)
  def But(expect: Expectation[S, M,E]): Assertion[S, M, E] = add(Conjuction.But, expect)

  def description = AssertionDescription(flow.description, Descriptions(expectations.map(_.description)))

  override def toString = description.toString
}

object Expectation {
  def apply[S, M[_], E](description: Description, predicate: M[S] => Seq[TestResult[E]]): Expectation[S,M, E] = new Expectation[S, M,E](description, predicate, None)
}

class Expectation[S,  M[_], E](val description: Description, val predicate: M[S] => Seq[TestResult[E]], val filePosition: Option[FilePosition]) {

  private def add(conj: Conjuction, other: Expectation[S,M, E]) = new Expectation[S,M, E](description.add(other.description.prepend(conj)), state => {
    predicate(state) ++ other.predicate(state)
  }, filePosition)

  def And(other: Expectation[S,M, E]) = add(Conjuction.And, other)
  def But(other: Expectation[S,M, E]) = add(Conjuction.But, other)
  def -(other: Expectation[S,M, E]) = add(Conjuction.-, other)

  def copy(description: Description) = new Expectation[S,M, E](description, predicate, filePosition)

  override def toString = description.mkString("")
}
