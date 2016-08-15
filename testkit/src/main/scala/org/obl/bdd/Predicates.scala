package org.obl.bdd

import SelfDescribeF1._

object Predicates {
  
  def `equal to`[A](i:A):A => Boolean = selfDescribe { inp =>
    i == inp
  }
  
  def `not equal to`[A](i:A):A => Boolean = selfDescribe { inp =>
    i != inp
  }
  
}