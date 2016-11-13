package com.github.gdefacci.bdd
package testkit

import org.scalatest.FunSuite

class AssertionRunnerTest extends FunSuite {

  def fail[T](msg: String): T = throw new RuntimeException(msg)

  type Id[T] = T
  
  test("ok") {

    val assertion = Flow[String, String](Text("aa"), () => "str") And
      Step[String](Text("b"), a => "bb" + a) Then
      Step[String](Text("c"), a => "cc" + a) And
      Expectation[String, String](Text("exp"), s => Ok :: Nil)

    val evs = AssertionRunner.runEvents(assertion)

    println(evs.mkString("\n"))
    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev: SuccessRunEvent[_,_] => ev }).size == 4)

  }

  test("expectation failure") {

    val assertion = Flow[String, String](Text("aa"), () => "str") And
      Step[String](Text("b"), a => "bb" + a) Then
      Step[String](Text("c"), a => "cc" + a) And
      Expectation[String, String](Text("c"), s => Fail("fail") :: Nil)
      
    val evs = AssertionRunner.runEvents(assertion)

    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev: SuccessRunEvent[_, _] => ev }).size == 3)
    assert((evs.collect { case ev: ExpectationFailureRunEvent[_, _] => ev }).size == 1)

  }

  test("source fails") {

    val assertion = Flow[String, String](Text("aa"), () => fail("source")) And
      Step[String](Text("b"), a => "bb" + a) Then
      Step[String](Text("c"), a => "cc" + a) And
      Expectation[String, String](Text("c"), s => Ok :: Nil)
      
    val evs = AssertionRunner.runEvents(assertion)

    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev: SuccessRunEvent[_, _] => ev }).size == 0)
    assert((evs.collect { case ev: SourceErrorRunEvent[_,_] => ev }).size == 1)

  }

  test("step fails") {

    val assertion = Flow[String, String](Text("aa"), () => "str") And
      Step[String](Text("b"), (a:String) => fail("step")) Then
      Step[String](Text("c"), a => "cc" + a) And
      Expectation[String, String](Text("c"), s => Ok :: Nil)
      
    val evs = AssertionRunner.runEvents(assertion)

    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev: SuccessRunEvent[_, _] => ev }).size == 1)
    assert((evs.collect { case ev: StepErrorRunEvent[_,_] => ev }).size == 1)

  }

  test("expectation fails") {

    val assertion = Flow[String, String](Text("aa"), () => "str") And
      Step[String](Text("b"), a => "bb" + a) Then
      Step[String](Text("c"), a => "cc" + a) And
      Expectation[String, String](Text("c"), s => fail("expectation"))
      
    val evs = AssertionRunner.runEvents(assertion)

    assert(evs.distinct.length == evs.length)
    assert((evs.collect { case ev: SuccessRunEvent[_, _] => ev }).size == 3)
    assert((evs.collect { case ev: ExpectationErrorRunEvent[_,_] => ev }).size == 1)

  }

}