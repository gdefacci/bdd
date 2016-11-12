package com.github.gdefacci.bdd

import language.higherKinds
import language.experimental.macros

trait BDD[A, M[_], E] {

  type Step = com.github.gdefacci.bdd.Step[A, M]
  type Source = com.github.gdefacci.bdd.Flow[A, M, E]
  type Expectation = com.github.gdefacci.bdd.Expectation[A, M, E]

  def source(f: () => A): Source = macro StepMacro.source[A, M, E]
  def step(f: A => M[A]): Step = macro StepMacro.step[A, M]

  def expectation(f: M[A] => TestResult[E]): Expectation = macro StepMacro.expectation[A, E, M]
  
  def expectations(f: M[A] => Seq[TestResult[E]]): Expectation = macro StepMacro.expectations[A, E, M]
  
  def scenario(f: Source): Scenario[A, M, E] = macro StepMacro.scenario[A, M, E]

  def scenario(description: String, f: Source): Scenario[A, M, E] = macro StepMacro.scenarioDescriptionProvided[A, M, E]

}

//trait DisjBDD[A, D[_,_], DE, E] {
//
//  type Disj[T] = D[DE, T] 
//  
//  type Step = com.github.gdefacci.bdd.Step[A, Disj]
//  type Source = com.github.gdefacci.bdd.Flow[A, Disj]
//  type Expectation = com.github.gdefacci.bdd.Expectation[A, Disj, E]
//
//  def source(f: () => A): Source = macro StepMacro.sourceDisj[A, DE, D]
//  def step(f: A => Disj[A]): Step = macro StepMacro.step[A, Disj]
//
//  def expectation(f: Disj[A] => TestResult[E]): Expectation = macro StepMacro.expectation[A, E, Disj]
//  
//  def expectations(f: Disj[A] => Seq[TestResult[E]]): Expectation = macro StepMacro.expectations[A, E, Disj]
//  
//  def scenario(f: Assertion[A, Disj, E]): Scenario[A, Disj, E] = macro StepMacro.scenario[A, Disj, E]
//
//  def scenario(description: String, f: Assertion[A, Disj, E]): Scenario[A, Disj, E] = macro StepMacro.scenarioDescriptionProvided[A, Disj, E]
//
//}