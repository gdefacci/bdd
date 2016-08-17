package org.obl.bdd

sealed trait TestResult[+E]

case object Ok extends TestResult[Nothing]
final case class Fail[E](err: E) extends TestResult[E]

case class Source[S](description: Description, initialValue: () => S, steps: Seq[Step[S]] = Nil) {

  def But(step: Step[S]): Source[S] = add(Conjuction.But, step)
  def When(step: Step[S]): Source[S] = add(Conjuction.When, step)
  def And(step: Step[S]): Source[S] = add(Conjuction.And, step)
  def Then(step: Step[S]) = add(Conjuction.Then, step)
  def -(step: Step[S]): Source[S] = add(Conjuction.-, step)

  private def add(conj: Conjuction, step: Step[S]): Source[S] =
    Source(description.add(step.description.prepend(conj)), initialValue, steps :+ step)

  private def add[E](conj: Conjuction, expect: Expectation[S, E]) =
    new Assertion[S, E](this, expect.copy(description = expect.description.prepend(conj)) :: Nil)

  def And[E](expect: Expectation[S, E]) = add(Conjuction.And, expect)
  def Then[E](expect: Expectation[S, E]) = add(Conjuction.Then, expect)
  def -[E](expect: Expectation[S, E]) = add(Conjuction.-, expect)

  override def toString = description.mkString("")
}

case class Step[S](description: Description, run: S => S) {

  override def toString = description.mkString("")

}

case class AssertionDescription(source: Description, expectations: Description) {

  override def toString = source.mkString("\n") + "\n" + expectations.mkString("\n")
}

case class Assertion[S, E](source: Source[S], expectations: Seq[Expectation[S, E]]) {

	private def add(conj: Conjuction, expect: Expectation[S, E]): Assertion[S, E] = 
	  Assertion[S, E](source, expectations :+ expect.copy( description = expect.description.prepend(conj)))
  
	def And(expect: Expectation[S, E]): Assertion[S, E] = add(Conjuction.And, expect)
  def But(expect: Expectation[S, E]): Assertion[S, E] = add(Conjuction.But, expect)

  def description = AssertionDescription(source.description, Descriptions(expectations.map(_.description)))

  override def toString = description.toString
}

case class Expectation[S, E](description: Description, predicate: S => Seq[TestResult[E]]) {

  private def add(conj: Conjuction, other: Expectation[S, E]) = Expectation[S, E](description.add(other.description.prepend(conj)), state => {
    predicate(state) ++ other.predicate(state)
  })

  def And(other: Expectation[S, E]) = add(Conjuction.And, other)
  def But(other: Expectation[S, E]) = add(Conjuction.But, other)
  def -(other: Expectation[S, E]) = add(Conjuction.-, other)

  override def toString = description.mkString("")
}

