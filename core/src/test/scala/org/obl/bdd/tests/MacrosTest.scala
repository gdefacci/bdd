package org.obl.bdd
package tests

import org.scalatest.FunSuite
import org.obl.bdd.BDD

class MacrosTest extends FunSuite {
  
  test("Source description") {
    val bdd = new BDD[Int,String] {}
    
    val src = bdd.source( () => 1 )
   
    assert(src.description == Text("src"))
  }
  
  test("Step description") {
    val bdd = new BDD[Int,String] {}
    
    val step = bdd.step( i => 1 )
   
    assert(step.description == Text("step"))
  }
  
  test("Expectation description") {
    val bdd = new BDD[Int,String] {}
    
    val expectation = bdd.expectation( i => Ok )
   
    assert(expectation.description == Text("expectation"))
  }
  
  test("SelfDescribeF1 description") {
    
    val f1 = SelfDescribeF1.selfDescribe[Int,Int]( i => i )
   
    assert(f1.toString == "f1")
  }
  
  test("Predicate description") {
    
    val predicate = Predicate.predicate[Int]( i => true )
   
    assert(predicate.description == Text("predicate"))
  }

}