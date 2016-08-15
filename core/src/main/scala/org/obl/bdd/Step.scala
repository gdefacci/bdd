package org.obl.bdd

sealed trait TestResult[+E]

case object Ok extends TestResult[Nothing]
final case class Fail[E](err: E) extends TestResult[E]

case class Source[S](description: Description, initialValue: () => S, steps: Seq[Step[S]] = Nil) {

  def But(step: Step[S]): Source[S] = add(step, Conjuction.But)
  def When(step: Step[S]): Source[S] = add(step, Conjuction.When)
  def And(step: Step[S]): Source[S] = add(step, Conjuction.And)
  def Then(step: Step[S]) = add(step, Conjuction.Then)
  def -(step: Step[S]): Source[S] = add(step, Conjuction.-)

  private def add(step: Step[S], conj: Conjuction): Source[S] =
    Source(description.add(step.description.prepend(conj)), initialValue, steps :+ step)

  private def add[E](expect: Expectation[S, E], conj: Conjuction) =
    new Assertion[S, E](this, expect.copy(description = expect.description.prepend(conj)) :: Nil)

  def And[E](expect: Expectation[S, E]) = add(expect, Conjuction.And)
  def Then[E](expect: Expectation[S, E]) = add(expect, Conjuction.Then)
  def -[E](expect: Expectation[S, E]) = add(expect, Conjuction.-)

  override def toString = description.mkString("")
}

case class Step[S](description: Description, run: S => S) {

  override def toString = description.mkString("")

}

case class AssertionDescription(source: Description, expectations: Description) {

  override def toString = source.mkString("\n") + "\n" + expectations.mkString("\n")
}

case class Assertion[S, E](source: Source[S], expectations: Seq[Expectation[S, E]]) {

  def And(expect: Expectation[S, E]): Assertion[S, E] = Assertion[S, E](source, expectations :+ expect)

  def description = AssertionDescription(source.description, Descriptions(expectations.map(_.description)))

  override def toString = description.toString
}

case class Expectation[S, E](description: Description, predicate: S => Seq[TestResult[E]]) {

  private def add(other: Expectation[S, E], conj:Conjuction) = Expectation[S, E](description.add(other.description.prepend(conj)), state => {
    predicate(state) ++ other.predicate(state)
  })

  def And(other: Expectation[S, E]) = add(other, Conjuction.And)
  def - (other: Expectation[S, E]) = add(other, Conjuction.-)

  override def toString = description.mkString("")
}

