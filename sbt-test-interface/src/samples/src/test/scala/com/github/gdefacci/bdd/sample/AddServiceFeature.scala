package com.github.gdefacci.bdd
package sample

trait SumService {
  def sum(lst: List[Int]): Int
}

case class AddServiceTestState(calculatorService: SumService, input: List[Int], result: Option[Int])

trait AddServiceSteps extends App with BDD[AddServiceTestState, String] {

  def createService = new SumService {
    def sum(lst: List[Int]): Int = lst.sum
  }

  def `Given the initial number`(input: Int): Source = source { () =>
    AddServiceTestState(createService, List(input), None)
  }

  def `adding number`(input: Int): Step = step { state =>
    state.copy(input = state.input :+ input)
  }

  def `the service is run`: Step = step { state =>
    state.copy(result = Some(state.calculatorService.sum(state.input)))
  }

  def `the result is`(n: Int): Expectation = expectation { state =>
    if (state.result.exists(_ == n)) Ok
    else Fail(s"Expecting $n but ${state.result.map(n => s"got $n").getOrElse("no result has been computed")}")
  }
}

object AddServiceFeatures extends AddServiceSteps with Features {

  lazy val `can add 2 numbers` = scenario(
    `Given the initial number`(10)
      And `adding number`(4)
      When `the service is run`
      Then `the result is`(14))

  lazy val `can add 3 numbers` = scenario(
    `Given the initial number`(10)
      And `adding number`(4)
      And `adding number`(9)
      When `the service is run`
      Then `the result is`(23))

  lazy val features = new Feature("add numbers",
    `can add 2 numbers`,
    `can add 3 numbers`) :: Nil

}