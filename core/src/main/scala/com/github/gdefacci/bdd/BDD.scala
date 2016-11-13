package com.github.gdefacci.bdd

import language.experimental.macros

trait BDD[A, E] {

  type Step = com.github.gdefacci.bdd.Step[A]
  type Source = com.github.gdefacci.bdd.Flow[A, E]
  type Expectation = com.github.gdefacci.bdd.Expectation[A, E]

  def source(f: () => A): Source = macro StepMacro.source[A, E]
  def step(f: A => A): Step = macro StepMacro.step[A]

  def expectation(f: A => TestResult[E]): Expectation = macro StepMacro.expectation[A, E]
  
  def expectations(f: A => Seq[TestResult[E]]): Expectation = macro StepMacro.expectations[A, E]
  
  def scenario(f: Source): Scenario[A, E] = macro StepMacro.scenario[A, E]

  def scenario(description: String, f: Source): Scenario[A, E] = macro StepMacro.scenarioDescriptionProvided[A, E]

}