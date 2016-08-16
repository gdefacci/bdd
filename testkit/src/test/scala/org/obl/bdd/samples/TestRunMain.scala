package org.obl.bdd
package samples

import java.io.PrintWriter

object TestRunMain extends App {

  val features: Seq[Feature[_, _]] = Seq(CashWithdrawal, CalculatorFeature)

  val featureRunner = FeatureRunner(DefaultScenarioRunner)

  new ConsoleReporter(new PrintWriter(System.out), featureRunner).runFeatures(features)

}