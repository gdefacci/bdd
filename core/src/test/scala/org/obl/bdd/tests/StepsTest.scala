package org.obl.bdd
package tests

import org.scalatest.FunSuite

class StepsTest extends FunSuite {
  
  test("Step description 1") {
    val src = Source(Text("a"), () => "aa") and Step(Text("b"), a => "bb"+a)
    
    assert(src.description.mkString("-") == "a and-b")
    
    val src1 = Source(Text("a"), () => "aa") but Step(Text("b"), a => "bb"+a)
    
    assert(src1.description.mkString("-") == "a but-b")

  }
  
  test("Step description 2") {
    val src = Source(Text("a"), () => "aa") and Step(Text("b"), a => "bb"+a) Then Action[String](Text("c"), a => "bb"+a) and
                    Expectation[String,String](Text("e"), s => Ok :: Nil)
    
    assert(src.description.source.mkString("-") == "a and-b then")
    assert(src.description.action.mkString("-") == "c and")
    assert(src.description.expectations.mkString("-") == "e")
  }
  
}