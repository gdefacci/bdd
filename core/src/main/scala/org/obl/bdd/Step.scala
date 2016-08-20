package org.obl.bdd

sealed trait TestResult[+E]

case object Ok extends TestResult[Nothing]
final case class Fail[E](err: E) extends TestResult[E]

object Source {
  def apply[S](description: Description, initialValue: () => S):Source[S] = new Source[S](description, initialValue, Nil, None)
}

class Source[S](val description: Description, val initialValue: () => S, val steps: Seq[Step[S]], val filePosition:Option[FilePosition]) {
  
  def this(description: Description, initialValue: () => S, filePosition:Option[FilePosition]) = this(description, initialValue, Nil, filePosition)

  def But(step: Step[S]): Source[S] = add(Conjuction.But, step)
  def When(step: Step[S]): Source[S] = add(Conjuction.When, step)
  def And(step: Step[S]): Source[S] = add(Conjuction.And, step)
  def Then(step: Step[S]) = add(Conjuction.Then, step)
  def -(step: Step[S]): Source[S] = add(Conjuction.-, step)

  private def add(conj: Conjuction, step: Step[S]): Source[S] =
    new Source(description.add(step.description.prepend(conj)), initialValue, steps :+ step, filePosition)

  private def add[E](conj: Conjuction, expect: Expectation[S, E]) =
    new Assertion[S, E](this, expect.copy(description = expect.description.prepend(conj)) :: Nil)

  def And[E](expect: Expectation[S, E]) = add(Conjuction.And, expect)
  def Then[E](expect: Expectation[S, E]) = add(Conjuction.Then, expect)
  def -[E](expect: Expectation[S, E]) = add(Conjuction.-, expect)

  override def toString = description.mkString("")
}

object Step {
  def apply[S](description: Description, run: S => S):Step[S] = new Step[S](description, run, None)
}

class Step[S](val description: Description, val run: S => S, val filePosition:Option[FilePosition]) {

  def copy(description: Description) = new Step[S](description, run, filePosition)
    
  override def toString = description.mkString("")

}

case class AssertionDescription(source: Description, expectations: Description) {

  override def toString = source.mkString("\n") + "\n" + expectations.mkString("\n")
}

class Assertion[S, E](val source: Source[S], val expectations: Seq[Expectation[S, E]]) {

	private def add(conj: Conjuction, expect: Expectation[S, E]): Assertion[S, E] = 
	  new Assertion[S, E](source, expectations :+ expect.copy( description = expect.description.prepend(conj)))
  
	def And(expect: Expectation[S, E]): Assertion[S, E] = add(Conjuction.And, expect)
  def But(expect: Expectation[S, E]): Assertion[S, E] = add(Conjuction.But, expect)

  def description = AssertionDescription(source.description, Descriptions(expectations.map(_.description)))

  override def toString = description.toString
}

object Expectation{ 
  def apply[S, E](description: Description, predicate: S => Seq[TestResult[E]]):Expectation[S,E] = new Expectation[S,E](description, predicate, None)
}

class Expectation[S, E](val description: Description, val predicate: S => Seq[TestResult[E]], val filePosition:Option[FilePosition]) {

  private def add(conj: Conjuction, other: Expectation[S, E]) = new Expectation[S, E](description.add(other.description.prepend(conj)), state => {
    predicate(state) ++ other.predicate(state)
  }, filePosition)

  def And(other: Expectation[S, E]) = add(Conjuction.And, other)
  def But(other: Expectation[S, E]) = add(Conjuction.But, other)
  def -(other: Expectation[S, E]) = add(Conjuction.-, other)

  def copy(description: Description) = new Expectation[S, E](description, predicate, filePosition)
  
  override def toString = description.mkString("")
}

