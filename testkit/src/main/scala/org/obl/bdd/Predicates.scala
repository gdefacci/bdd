package org.obl.bdd

import Predicate._

object Predicates {
  
  def `equal to`[A](value:A):Predicate[A] = predicate { inp =>
    value == inp
  }
  
  def `not equal to`[A](value:A):Predicate[A] = predicate { inp =>
    value != inp
  }
  
  def `greater than`[A : Ordering](value:A):Predicate[A] = predicate { inp =>
    Ordering[A].gt(inp, value)
  }
  
  def `less than`[A : Ordering](value:A):Predicate[A] = predicate { inp =>
    Ordering[A].lt(inp, value)
  }
  
  def `greater or equal to`[A : Ordering](value:A):Predicate[A] = predicate { inp =>
    Ordering[A].gteq(inp, value)
  }
  
  def `less or equal to`[A : Ordering](value:A):Predicate[A] = predicate { inp =>
    Ordering[A].lteq(inp, value)
  }
  
  
}