package org.obl.bdd
package tests

import org.scalatest.FunSuite

class DefaultScenarioRunnerTest extends FunSuite {
  
  def fail[T](msg:String):T = throw new RuntimeException(msg)
  
  test("ok") {
    
    val assertion = Source(Text("aa"), () => "str") and 
                    Step(Text("b"), a => "bb"+a) Then 
                    Action(Text("c"), a => "cc"+a) and
                    Expectation[String,String](Text("c"), s => Ok :: Nil)
    val scenario = Scenario("scen1", assertion)
    
    val evs = DefaultScenarioRunner.run(scenario)
    
    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev:StartEvent[_,_] => ev }).size == 4)
    assert((evs.collect { case ev:SuccessEvent[_,_] => ev }).size == 4)
    
  }
  
  test("expectation failure") {
    
    val assertion = Source(Text("aa"), () => "str") and 
                    Step(Text("b"), a => "bb"+a) Then 
                    Action(Text("c"), a => "cc"+a) and
                    Expectation[String,String](Text("c"), s => Fail("fail") :: Nil)
    val scenario = Scenario("scen1", assertion)
    
    val evs = DefaultScenarioRunner.run(scenario)
    
    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev:StartEvent[_,_] => ev }).size == 4)
    assert((evs.collect { case ev:SuccessEvent[_,_] => ev }).size == 3)
    assert((evs.collect { case ev:ExpectationFailure[_,_] => ev }).size == 1)
    
  }
  
  test("source fails") {
    
    val assertion = Source[String](Text("aa"), () => fail("source")) and 
                    Step(Text("b"), a => "bb"+a) Then 
                    Action(Text("c"), a => "cc"+a) and
                    Expectation[String,String](Text("c"), s => Ok :: Nil)
    val scenario = Scenario("scen1", assertion)
    
    val evs = DefaultScenarioRunner.run(scenario)
    
    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev:StartEvent[_,_] => ev }).size == 1)
    assert((evs.collect { case ev:SuccessEvent[_,_] => ev }).size == 0)
    assert((evs.collect { case ev:SourceError[_,_] => ev }).size == 1)
    
  }
 
  test("step fails") {
    
    val assertion = Source(Text("aa"), () => "str") and 
                    Step(Text("b"), a => fail("step")) Then 
                    Action(Text("c"), a => "cc"+a) and
                    Expectation[String,String](Text("c"), s => Ok :: Nil)
    val scenario = Scenario("scen1", assertion)
    
    val evs = DefaultScenarioRunner.run(scenario)
    
    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev:StartEvent[_,_] => ev }).size == 2)
    assert((evs.collect { case ev:SuccessEvent[_,_] => ev }).size == 1)
    assert((evs.collect { case ev:StepError[_,_] => ev }).size == 1)
    
  }
  
  test("action fails") {
    
    val assertion = Source(Text("aa"), () => "str") and 
                    Step(Text("b"), a => "bb"+a) Then 
                    Action(Text("c"), a => fail("action")) and
                    Expectation[String,String](Text("c"), s => Ok :: Nil)
    val scenario = Scenario("scen1", assertion)
    
    val evs = DefaultScenarioRunner.run(scenario)
    
    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev:StartEvent[_,_] => ev }).size == 3)
    assert((evs.collect { case ev:SuccessEvent[_,_] => ev }).size == 2)
    assert((evs.collect { case ev:ActionError[_,_] => ev }).size == 1)
    
  }

  test("expectation fails") {
    
    val assertion = Source(Text("aa"), () => "str") and 
                    Step(Text("b"), a => "bb"+a) Then 
                    Action(Text("c"), a => "cc"+a) and
                    Expectation[String,String](Text("c"), s => fail("expectation"))
    val scenario = Scenario("scen1", assertion)
    
    val evs = DefaultScenarioRunner.run(scenario)
    
    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev:StartEvent[_,_] => ev }).size == 4)
    assert((evs.collect { case ev:SuccessEvent[_,_] => ev }).size == 3)
    assert((evs.collect { case ev:ExpectationError[_,_] => ev }).size == 1)
    
  }

  
}