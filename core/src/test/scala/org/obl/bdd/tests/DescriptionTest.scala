package org.obl.bdd
package tests

import org.scalatest.FunSuite

class DescriptionTest extends FunSuite {
  
  test("append") {
    val sample1 = Descriptions(Text("aa") :: Descriptions(Text("bb") :: Text("cc") :: Nil) :: Nil)
    
    assert(Descriptions(Text("aa") :: Descriptions(Text("bb") :: Text("cc",Some(Conjuction.And)) :: Nil) :: Nil) == sample1.append(Conjuction.And))
  }

  test("mkString") {
    val sample1 = Descriptions(Text("aa") :: Descriptions(Text("bb") :: Text("cc") :: Nil) :: Nil)
    
    assert("aa--bb--cc" == sample1.mkString("--"))
    assert("aa" == Text("aa").mkString("--"))
    
  }
  
}