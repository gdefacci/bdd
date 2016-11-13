package com.github.gdefacci

import language.experimental.macros

package object bdd {

  def selfDescribe[A, B](f: A => B): A => B = macro StepMacro.selfDescribe[A, B]
  def predicate[A](f: A => Boolean): Predicate[A] = macro StepMacro.predicate[A]

  def withPosition[A](f:FilePosition => A):A = macro StepMacro.withPosition
  def withDescription[A](f:String => A):A = macro StepMacro.withDescription
  def withDescriptionAndPosition[A](f:(String, FilePosition) => A):A = macro StepMacro.withDescriptionAndPosition
  
}