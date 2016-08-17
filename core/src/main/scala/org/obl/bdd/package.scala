package org.obl

import language.experimental.macros

package object bdd {
  
    def selfDescribe[A,B](f:A => B):A => B = macro StepMacro.selfDescribe[A,B]
    def predicate[A](f:A => Boolean):Predicate[A] = macro StepMacro.predicate[A]
    
}