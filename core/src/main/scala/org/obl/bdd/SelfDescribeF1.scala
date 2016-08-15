package org.obl.bdd

import language.experimental.macros

class SelfDescribeF1[A,B](description:String, function:A => B) extends (A => B) {
  def apply(a:A):B = function(a)
  override def toString = description
}

object SelfDescribeF1 {
  
  def selfDescribe[A,B](f:A => B):A => B = macro StepMacro.selfDescribe[A,B]
  
}
