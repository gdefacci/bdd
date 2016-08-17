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
    
    val f1 = selfDescribe[Int,Int]( i => i )
   
    assert(f1.toString == "f1")
  }
  
  test("Predicate description") {
    
    val pred = predicate[Int]( i => true )
   
    assert(pred.description == Text("pred"))
  }

   test("Scenario description") {
    
    val scenario1 = scenario[Int,Int]( new Assertion[Int,Int](new Source[Int](Text(""), () => 12), Nil ) )
   
    assert(scenario1.title == "scenario1")
  }
   
  test("Scenario description 1") {
    
    val scenario1 = {
      val i = 12
      scenario[Int,Int]( new Assertion[Int,Int](new Source[Int](Text(""), () => i), Nil ) )
    }
   
    assert(scenario1.title == "scenario1")
  }
   
   
 
}