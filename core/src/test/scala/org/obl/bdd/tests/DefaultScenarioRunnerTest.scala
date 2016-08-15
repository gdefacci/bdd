package org.obl.bdd
package tests

import org.scalatest.FunSuite

class DefaultScenarioRunnerTest extends FunSuite {

  def fail[T](msg: String): T = throw new RuntimeException(msg)

  test("ok") {

    val assertion = Source(Text("aa"), () => "str") And
      Step[String](Text("b"), a => "bb" + a) Then
      Step[String](Text("c"), a => "cc" + a) And
      Expectation[String, String](Text("c"), s => Ok :: Nil)
    val scenario = Scenario("scen1", assertion)

    val evs = DefaultScenarioRunner.run(scenario)

    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev: StartEvent[_, _] => ev }).size == 4)
    assert((evs.collect { case ev: SuccessEvent[_, _] => ev }).size == 4)

  }

  test("expectation failure") {

    val assertion = Source(Text("aa"), () => "str") And
      Step[String](Text("b"), a => "bb" + a) Then
      Step[String](Text("c"), a => "cc" + a) And
      Expectation[String, String](Text("c"), s => Fail("fail") :: Nil)
      
    val scenario = Scenario("scen1", assertion)

    val evs = DefaultScenarioRunner.run(scenario)

    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev: StartEvent[_, _] => ev }).size == 4)
    assert((evs.collect { case ev: SuccessEvent[_, _] => ev }).size == 3)
    assert((evs.collect { case ev: ExpectationFailure[_, _] => ev }).size == 1)

  }

  test("source fails") {

    val assertion = Source[String](Text("aa"), () => fail("source")) And
      Step[String](Text("b"), a => "bb" + a) Then
      Step[String](Text("c"), a => "cc" + a) And
      Expectation[String, String](Text("c"), s => Ok :: Nil)
      
    val scenario = Scenario("scen1", assertion)

    val evs = DefaultScenarioRunner.run(scenario)

    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev: StartEvent[_, _] => ev }).size == 1)
    assert((evs.collect { case ev: SuccessEvent[_, _] => ev }).size == 0)
    assert((evs.collect { case ev: SourceError[_, _] => ev }).size == 1)

  }

  test("step fails") {

    val assertion = Source(Text("aa"), () => "str") And
      Step[String](Text("b"), a => fail("step")) Then
      Step[String](Text("c"), a => "cc" + a) And
      Expectation[String, String](Text("c"), s => Ok :: Nil)
      
    val scenario = Scenario("scen1", assertion)

    val evs = DefaultScenarioRunner.run(scenario)

    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev: StartEvent[_, _] => ev }).size == 2)
    assert((evs.collect { case ev: SuccessEvent[_, _] => ev }).size == 1)
    assert((evs.collect { case ev: StepError[_, _] => ev }).size == 1)

  }

  test("expectation fails") {

    val assertion = Source(Text("aa"), () => "str") And
      Step[String](Text("b"), a => "bb" + a) Then
      Step[String](Text("c"), a => "cc" + a) And
      Expectation[String, String](Text("c"), s => fail("expectation"))
      
    val scenario = Scenario("scen1", assertion)

    val evs = DefaultScenarioRunner.run(scenario)

    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev: StartEvent[_, _] => ev }).size == 4)
    assert((evs.collect { case ev: SuccessEvent[_, _] => ev }).size == 3)
    assert((evs.collect { case ev: ExpectationError[_, _] => ev }).size == 1)

  }

}