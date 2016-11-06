package com.github.gdefacci.bdd

class SelfDescribeF1[A,B](description:String, function:A => B, filePosition:Option[FilePosition]) extends (A => B) {
  def apply(a:A):B = function(a)
  override def toString = description
}
