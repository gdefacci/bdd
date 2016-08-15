package org.obl.bdd
package samples

trait CalculatorService {
  def calculate(str: String): Int
}

case class CalculatorTestState(calculatorService: CalculatorService, input: String, result: Option[Int])

trait CalculatorServiceSteps extends App with BDD[CalculatorTestState, String] {

  def createCalculatorService: CalculatorService

  def `given the input`(input: String): Source = source { () =>
    CalculatorTestState(createCalculatorService, input, None)
  }

  def `the calculator is run`: Step = step { state =>
    state.copy(result = Some(state.calculatorService.calculate(state.input)))
  }

  def `the output should be`(result: Int): Expectation = expectation { state =>
    if (state.result.exists(_ == result)) Ok
    else Fail(s"expecting $result got ${state.result}")
  }

}

object DummyCalculatorServiceSteps extends CalculatorServiceSteps {
  def createCalculatorService: CalculatorService = new DummyCalculatorService
}

import DummyCalculatorServiceSteps._

object CalculatorFeature extends Feature[CalculatorTestState, String](
  "Calculator Feature",
  Scenario("Add 2 numbers",
    `given the input`("2+2") when
      `the calculator is run` `then`
      `the output should be`(4)),
  OutlineScenario(
    "Some expressions",
    
    ("3+4", 7) ::
    ("3*4+7/2*15", 57) ::
    ("98+1", 99) :: Nil) {
      
      case (input, output) =>
        
        Scenario(s"Calculate expression $input",
          `given the input`(input) Then
            `the calculator is run` and
            `the output should be`(output))
    })
    
    
class DummyCalculatorService extends CalculatorService {

  def normalizeTokens(arr: Seq[String]) = arr.map(_.trim).filter(_.nonEmpty).toList

  def parse(str: String) = {
    val rgOps = """[\+\-\*\/]"""
    val rgNum = """\d+"""
    normalizeTokens(str.split(rgOps)).map(_.toInt) -> normalizeTokens(str.split(rgNum))
  }

  def doOp(a: Int, b: Int, str: String): Int = str match {
    case "*" => a * b
    case "/" => a / b
    case "+" => a + b
    case "-" => a - b
  }

  def subst(nums: Seq[Int], ops: Seq[String], opSet: Set[String]) = {
    val z: (List[Int], List[String]) = List(nums.head) -> Nil
    nums.tail.zip(ops).foldLeft(z) { (lst, itm) =>
      val (nums, ops) = lst
      val (num, op) = itm
      if (opSet.contains(op)) {
        (nums.init :+ doOp(nums.last, num, op)) -> ops
      } else {
        (nums :+ num) -> (ops :+ op)
      }
    }
  }

  def calculate(str: String) = {
    val (nums, ops) = parse(str)
    val (nums1, ops1) = subst(nums, ops, Set("/", "*"))
    val (nums2, ops2) = subst(nums1, ops1, Set("+", "-"))
    assert(ops2.isEmpty)
    assert(nums2.length == 1)
    nums2.head
  }
}

    