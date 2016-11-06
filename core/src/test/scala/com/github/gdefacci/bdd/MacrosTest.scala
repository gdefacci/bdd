package com.github.gdefacci.bdd

import org.scalatest.FunSuite

object Id {
  
  type Id[T] = T
  
}

class MacrosTest extends FunSuite {
  
  import Id.Id
  
  test("Source description") {
    val bdd = new BDD[Int,Id,String] {}
    
    val src = bdd.source( () => 1 )
   
    assert(src.description == Text("src"))
  }
  
  test("Step description") {
    val bdd = new BDD[Int,Id,String] {}
    
    val step = bdd.step( i => 1 )
   
    assert(step.description == Text("step"))
  }
  
  test("Expectation description") {
    val bdd = new BDD[Int,Id,String] {}
    
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
    
    val bdd = new BDD[Int, Id, Int] {} 
     
    val scenario1 = bdd.scenario( new Assertion[Int,Id,Int](Flow[Int,Id](Text(""), () => 12), Nil ) )
   
    assert(scenario1.title == "scenario1")
  }
   
  test("Scenario description 1") {
     val bdd = new BDD[Int, Id, Int] {} 

    val scenario1 = {
      val i = 12
      bdd.scenario( new Assertion[Int, Id,Int](Flow[Int, Id](Text(""), () => i), Nil ) )
    }
   
    assert(scenario1.title == "scenario1")
  }
   
  test("withDescription") {
    
    val scenario1 = withDescription { str => str }
   
    assert(scenario1 == "scenario1")
  }
  
  test("with Description and pos") {
    
    val scenario1 = withDescriptionAndPosition[String] { (str, pos) => str }
   
    assert(scenario1 == "scenario1")
  } 
 
}