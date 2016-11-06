package com.github.gdefacci.bdd
package testkit

object Descriptions {

  def subjectDescription(subj: EventSubject) = subj match {
    case SourceSubject(i, pos) => s"""The step "${i.mkString("")}""""
    case StepSubject(i, pos) => s"""The step "${i.mkString("")}""""
    case ExpectationSubject(i, pos) => s"""The expectation "${i.mkString("")}""""
  }

  private def subjectPosition(subj: EventSubject) = subj match {
    case SourceSubject(i, pos) => pos
    case StepSubject(i, pos) => pos
    case ExpectationSubject(i, pos) => pos
  }

  def getInput(ev: ErrorEvent): Option[Any] = ev match {
    case e: StepError => Some(e.input)
    case e: ExpectationError => Some(e.input)
    case e: ExpectationFailure => Some(e.input)
    case _ => None
  }

  def getMessage(ev: ErrorEvent): String = ev match {
    case e: ExceptionEvent => 
      e.exception.getMessage + "\n\n" + e.exception.getStackTrace.mkString("\n") + "\n"
    case e: ExpectationFailure => 
      e.failures.mkString(", ")
  }

  def errorDescription(err: ErrorEvent) = {
    err match {
      case ev: ExceptionEvent =>
        s"${subjectDescription(ev.subject)}${getInput(ev).map(i => s" with state $i ").getOrElse("")}caused error ${getMessage(err)}"
      case ev: ExpectationFailure =>
        s"Error: \n${ev.failures.mkString("\n")}\n${subjectDescription(ev.subject)} failed with input ${ev.input}"
    }
  }
  
  def failedScenario(sc:ScenarioRunResult) = {
    val errTitle = s"  Failed scenario : ${sc.title}"
    val sepCount: Int = errTitle.length + 4
    val sep = "-" * sepCount
s"""
$sep
$errTitle
$sep
${sc.description.mkString("\n")}
${sc.events.collect { case ev:ErrorEvent => 
  s"""${errorDescription(ev)}
  ${ev.subject.position.map(ln => s"($ln)\n").getOrElse("\n")}
  """
}.mkString("\n")}
${sc.filePosition.map(ln => s"Scenario ($ln)\n").getOrElse("")}
"""
  }

}