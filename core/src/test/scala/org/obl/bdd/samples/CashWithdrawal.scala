package org.obl.bdd
package samples

case class CashWithdrawalService(private var deposited: Int = 0) {
  def deposit(amount: Int) = {
    deposited += amount
  }

  def depositedAmount = deposited

  def dispense(amount: Int) = {
    if (deposited > amount) {
      deposited -= amount
      amount
    } else {
      val r = deposited
      deposited = 0
      r
    }
  }
}

case class CashWithdrawalTestState(service: CashWithdrawalService, dispensed: Option[Int])

trait CashWithdrawalSteps extends BDD[CashWithdrawalTestState, String] {

  def `Given i have deposited in my account`(amount: Int): Source = source { () =>
    val service = new CashWithdrawalService()
    service.deposit(amount)
    CashWithdrawalTestState(service, None)
  }

  def `i request`(amount: Int): Step = step { state =>
    val dispensed = state.service.dispense(amount)
    state.copy(dispensed = Some(dispensed))
  }

  def `should be dispensed`(amount: Int): Expectation = expectation { state =>
    if (state.dispensed.get == amount) Ok
    else Fail(s"expecting dispensed $amount but was ${state.dispensed.get}")
  }

}

object CashWithdrawalSteps extends CashWithdrawalSteps

import CashWithdrawalSteps._

case class CashWithdrawalExample(deposit: Int, requested: Int, expectedDispensed: Int)

object CashWithdrawal extends Feature(
  "Cash withdrawal",
  Scenario("Successful withdrawal from an account in credit",
    `Given i have deposited in my account`(100)
      When `i request`(20)
      Then `should be dispensed`(20)),
  Scenario("Successful withdrawal from an account in debit",
    `Given i have deposited in my account`(10)
      When `i request`(20)
      Then `should be dispensed`(15)),
  Scenario("Successful withdrawal from an account in debit using -",
    `Given i have deposited in my account`(10)
      - `i request`(20)
      - `should be dispensed`(10)),
  OutlineScenario("Few withdrawal",

    CashWithdrawalExample(20, 20, 20) ::
      CashWithdrawalExample(0, 20, 0) ::
      CashWithdrawalExample(10, 20, 10) ::
      CashWithdrawalExample(200, 20, 20) :: Nil) {
      case CashWithdrawalExample(deposit, requested, expectedDispensed) =>

        Scenario(s"Withdrawal with deposit $deposit requesting $requested",
          `Given i have deposited in my account`(deposit)
            When `i request`(requested)
            Then `should be dispensed`(expectedDispensed))
    })