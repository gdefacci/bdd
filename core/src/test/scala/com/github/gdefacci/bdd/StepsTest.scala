package com.github.gdefacci.bdd

import org.scalatest.FunSuite

class StepsTest extends FunSuite {

  type Id[T] = T
  
  test("Step description 1") {
    val src = Flow[String, Id, String](Text("a"), () => "aa") And Step[String, Id](Text("b"), a => "bb" + a)

    assert(src.description.mkString("-") == "a-and b")

    val src1 = Flow[String, Id, String](Text("a"), () => "aa") But Step[String, Id](Text("b"), a => "bb" + a)

    assert(src1.description.mkString("-") == "a-but b")

  }

  test("Step description 2") {
    val src = Flow[String, Id, String](Text("a"), () => "aa") And Step[String, Id](Text("b"), a => "bb" + a) Then Step[String, Id](Text("c"), a => "bb" + a) And
      Expectation[String, Id, String](Text("e"), s => Ok :: Nil)

    assert(src.description.mkString("-") == "a-and b-then c-and e")
  }

  test("Step description 3") {
    val src = Flow[String, Id, String](Text("a"), () => "aa") And
      Expectation[String, Id, String](Text("e"), s => Ok :: Nil) But Expectation[String, Id, String](Text("b"), s => Ok :: Nil) 

    assert(src.description.mkString("-") == "a-and e-but b")
  }

}