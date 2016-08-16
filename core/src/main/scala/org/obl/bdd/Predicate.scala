package org.obl.bdd

import language.experimental.macros

class Predicate[A](val description:Description, predicate:A => Boolean) extends (A => Boolean) {
  
  def apply(a:A):Boolean = predicate(a)
  override def toString = description.mkString(" ")
 
  def and(predicate:Predicate[A]):Predicate[A] = and(predicate, Conjuction.And)
  def but(predicate:Predicate[A]):Predicate[A] = and(predicate, Conjuction.But)
  
  private def and(predicate:Predicate[A], conj:Conjuction) =
    new Predicate[A](description.add(predicate.description.prepend(conj)), a => apply(a) && predicate(a))
  
  def or(predicate:Predicate[A]) =
    new Predicate[A](description.add(predicate.description.prepend(Conjuction.Or)), a => apply(a) || predicate(a))
  
}

object Predicate {
  
  def predicate[A](f:A => Boolean):Predicate[A] = macro StepMacro.predicate[A]
  
}