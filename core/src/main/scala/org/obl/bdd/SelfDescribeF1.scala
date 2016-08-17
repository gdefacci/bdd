package org.obl.bdd

class SelfDescribeF1[A,B](description:String, function:A => B) extends (A => B) {
  def apply(a:A):B = function(a)
  override def toString = description
}
