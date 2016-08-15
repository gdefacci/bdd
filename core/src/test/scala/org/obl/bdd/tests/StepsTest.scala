package org.obl.bdd
package tests

import org.scalatest.FunSuite

class StepsTest extends FunSuite {
  
  test("Step description 1") {
    val src = Source(Text("a"), () => "aa") And Step[String](Text("b"), a => "bb"+a)
    
    assert(src.description.mkString("-") == "a-and b")
    
    val src1 = Source(Text("a"), () => "aa") But Step(Text("b"), a => "bb"+a)
    
    assert(src1.description.mkString("-") == "a-but b")

  }
  
  test("Step description 2") {
    val src = Source(Text("a"), () => "aa") And Step[String](Text("b"), a => "bb"+a) Then Step[String](Text("c"), a => "bb"+a) And
                    Expectation[String,String](Text("e"), s => Ok :: Nil)
    
    assert(src.description.source.mkString("-") == "a-and b-then c")
    assert(src.description.expectations.mkString("-") == "and e")
  }
  
}