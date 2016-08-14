package org.obl.bdd

sealed trait TestResult[+E]

case object Ok extends TestResult[Nothing]
final case class Fail[E](err: E) extends TestResult[E]

case class Source[S](description: Description, initialValue: () => S, steps: Seq[Step[S]] = Nil) {

  def but(step: Step[S]): Source[S] = add(step, Conjuction.But)
  def when(step: Step[S]): Source[S] = add(step, Conjuction.When)
  def and(step: Step[S]): Source[S] = add(step, Conjuction.And)
  def +(step: Step[S]): Source[S] = add(step, Conjuction.+)

  private def add(step: Step[S], conj: Conjuction): Source[S] =
    Source(description.append(conj).add(step.description), initialValue, steps :+ step)

  private def add(action: Action[S], conj: Conjuction): AssertionFactory[S] =
    AssertionFactory[S](copy(description = description.append(conj)), action)

  def when(action: Action[S]) = add(action, Conjuction.When)
  def Then(action: Action[S]) = add(action, Conjuction.Then)
  def `then`(action: Action[S]) = add(action, Conjuction.Then)
  def +(action: Action[S]) = add(action, Conjuction.+)

  override def toString = description.mkString("")
}

case class Step[S](description: Description, run: S => S) {

  override def toString = description.mkString("")

}

case class Action[S](description: Text, run: S => S) {

  override def toString = description.value

  def append(suffix: Conjuction): Action[S] =
    Action(description.append(suffix), run)

}

case class AssertionFactory[S](source: Source[S], action: Action[S]) {

  private def add[E](expect: Expectation[S, E], sep: Conjuction) =
    new Assertion[S, E](source, action.append(sep), expect :: Nil)

  def and[E](expect: Expectation[S, E]) = add(expect, Conjuction.And)
  def Then[E](expect: Expectation[S, E]) = add(expect, Conjuction.Then)
  def `then`[E](expect: Expectation[S, E]) = add(expect, Conjuction.Then)
  def +[E](expect: Expectation[S, E]) = add(expect, Conjuction.+)

}

case class AssertionDescription(source: Description, action: Text, expectations: Description) {

  override def toString = source.add(action).mkString("\n") + "\n" + expectations.mkString("\n")
}

case class Assertion[S, E](source: Source[S], action: Action[S], expectations: Seq[Expectation[S, E]]) {

  def and(expect: Expectation[S, E]): Assertion[S, E] = Assertion[S, E](source, action, expectations :+ expect)

  def description = AssertionDescription(source.description, action.description, Descriptions(expectations.map(_.description)))

  override def toString = description.toString
}

case class Expectation[S, E](description: Description, predicate: S => Seq[TestResult[E]]) {

  private def add(other: Expectation[S, E], conj:Conjuction) = Expectation[S, E](description.append(conj).add(other.description), state => {
    predicate(state) ++ other.predicate(state)
  })

  def and(other: Expectation[S, E]) = add(other, Conjuction.And)
  def +(other: Expectation[S, E]) = add(other, Conjuction.+)

  override def toString = description.mkString("")
}

