package org.obl.bdd

import language.experimental.macros

trait BDD[A,E] {
  
  type State = A
  type Error = E
  type Source = org.obl.bdd.Source[A]
  type Step = org.obl.bdd.Step[A]
	type Action = org.obl.bdd.Action[A]
  type Expectation = org.obl.bdd.Expectation[A,E]
  
  def source(f:() => A):Source = macro StepMacro.source[A]
  def step(f:A => A):Step = macro StepMacro.step[A]
  def action(f:A => A):Action = macro StepMacro.action[A]
	def expectation(f:A => TestResult[E]):Expectation = macro StepMacro.expectation[A,E]
 
}