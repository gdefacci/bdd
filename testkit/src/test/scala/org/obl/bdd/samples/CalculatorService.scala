package org.obl.bdd
package samples

import Predicates._

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

  def `the result given by calculator is`(predicate: Int => Boolean): Expectation = expectation { state =>
    if (state.result.exists(predicate(_))) Ok
    else Fail(s"expecting result $predicate got ${state.result}")
  }

  def `the output should be`(result: Int): Expectation = expectation { state =>
    if (state.result.exists(_ == result)) Ok
    else Fail(s"expecting $result got ${state.result}")
  }

}

class CalculatorFeatures(impls: Seq[CalculatorServiceSteps]) {

  lazy val features: Seq[Feature[CalculatorTestState, String]] = impls.map { impl =>
    import impl._

    val `Add 2 numbers` = scenario(
        
      `given the input`("2+2")
        When `the calculator is run`
        Then `the output should be`(4)
    )
    
    val `Add 3 numbers` = scenario(
        
      `given the input`("2+2+4")
        When `the calculator is run`
        Then `the result given by calculator is`(`equal to`(6))
    )
    
    val `Add 3 numbers 2nd scenario` = scenario(
        
      `given the input`("2+2+4")
        When `the calculator is run`
        Then `the result given by calculator is`(`equal to`(6) or (`greater than`(8) but `less than`(10)))
    )

    new Feature[CalculatorTestState, String](
      "Calculator Feature",
      `Add 2 numbers`,
      `Add 3 numbers`,
      `Add 3 numbers 2nd scenario`,
      
      OutlineScenario(
        "Some expressions",

        ("3+4", 7) ::
          ("3*4+7/2*15", 57) ::
          ("98+1", 99) :: Nil) {

          case (input, output) =>

            Scenario(s"Calculate expression $input",

              `given the input`(input)
                Then `the calculator is run`
                And `the output should be`(output))
        })
  }

}

object CalculatorFeatures extends CalculatorFeatures(new CalculatorServiceSteps {
  def createCalculatorService: CalculatorService = new DummyCalculatorService
} :: Nil)


class DummyCalculatorService extends CalculatorService {

  private def normalizeTokens(arr: Seq[String]) = arr.map(_.trim).filter(_.nonEmpty).toList

  private case class Op(symbol: String, f: (Int, Int) => Int)
  private object Sum extends Op("+", _ + _)
  private object Sub extends Op("-", _ - _)
  private object Mul extends Op("*", _ * _)
  private object Div extends Op("/", _ / _)

  private val ops = Seq(Sum, Sub, Mul, Div)

  private def toOp(sym: String): Op = ops.find(_.symbol == sym).getOrElse(throw new RuntimeException("invalid operator " + sym))

  private def parse(str: String) = {
    val rgOps = "[" + ops.map(op => s"\\${op.symbol}").mkString + "]"
    val rgNum = """\d+"""
    normalizeTokens(str.split(rgOps)).map(_.toInt) -> normalizeTokens(str.split(rgNum)).map(toOp)
  }

  private def subst(nums: Seq[Int], ops: Seq[Op], opSet: Set[Op]) = {
    val z: (List[Int], List[Op]) = List(nums.head) -> Nil
    nums.tail.zip(ops).foldLeft(z) { (lst, itm) =>
      val (nums, ops) = lst
      val (num, op) = itm
      if (opSet.contains(op)) {
        (nums.init :+ op.f(nums.last, num)) -> ops
      } else {
        (nums :+ num) -> (ops :+ op)
      }
    }
  }

  def calculate(str: String) = {
    val (nums, ops) = parse(str)
    val (nums1, ops1) = subst(nums, ops, Set(Div, Mul))
    val (nums2, ops2) = subst(nums1, ops1, Set(Sum, Sub))
    assert(ops2.isEmpty)
    assert(nums2.length == 1)
    nums2.head
  }
}

    