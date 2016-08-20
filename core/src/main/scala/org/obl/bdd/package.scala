package org.obl

import language.experimental.macros

package object bdd {
  
    def selfDescribe[A,B](f:A => B):A => B = macro StepMacro.selfDescribe[A,B]
    def predicate[A](f:A => Boolean):Predicate[A] = macro StepMacro.predicate[A]

    def scenario[S,E](f: Assertion[S, E]): Scenario[S,E] = macro StepMacro.scenario[S, E]
    
    def scenario[S,E](description:String, f: Assertion[S, E]): Scenario[S,E] = macro StepMacro.scenarioDescriptionProvided[S, E]

}