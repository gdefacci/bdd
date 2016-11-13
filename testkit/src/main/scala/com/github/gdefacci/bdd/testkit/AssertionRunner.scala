package com.github.gdefacci.bdd
package testkit

import scala.util.Try
import scala.util.Failure
import scala.util.Success

object AssertionRunner {

  private def cron[T](action: => Try[T]):(Long, Long, Try[T]) = {
    val st = System.currentTimeMillis()
    val r = action
    val end = System.currentTimeMillis()
    (st, end, r)
  }
  
  private def onStep[S,E](step:FlowStep[S,E], ms:S):(S => S) => (List[ScenarioRunEvent[S,  E]], Try[Either[List[Fail[E]], S]]) = { run =>
    val stepSubj = StepSubject(step.description, step.filePosition)

    cron(Try(run(ms))) match {
      case (start, end, rm @ Success(v)) =>
        (Nil :+ StepSuccessRunEvent[S,  E](stepSubj, ms, v, start, end)) -> rm.map(Right(_))
      case (start, end, rm @ Failure(err)) =>
        (Nil :+ StepErrorRunEvent[S,  E](stepSubj, ms, err, start, end)) -> rm.map(Right(_))
    }
  }
  
  private def onExpectation[S,E](exp:FlowStep[S,E], ms:S):(S => Seq[TestResult[E]]) => (List[ScenarioRunEvent[S,  E]], Try[Either[List[Fail[E]], S]]) = { predicate =>
    val subj = ExpectationSubject(exp.description, exp.filePosition)
    cron(Try(predicate(ms))) match {
      case (start, end, Success(trs)) if trs.forall(_ == Ok) =>
        List(ExpectationSuccessRunEvent[S,  E](subj, ms, start, end)) -> Success(Right(ms))
      case (start, end, Success(trs)) =>
        val failures = trs.collect { case f @ Fail(e) => e }.toList
        List(ExpectationFailureRunEvent[S,  E](subj, ms, failures, start, end)) -> Success(Left(failures.map(Fail[E](_))))
      case (start, end, Failure(err)) =>
        List(ExpectationErrorRunEvent[S,  E](subj, ms, err, start, end)) -> Failure(err)
    }
  }

  def runEvents[S, E](flow: Flow[S,  E]):List[ScenarioRunEvent[S,  E]] = {
    run[S,E](flow)._1
  }
  
  def run[S, E](flow: Flow[S,  E]):(List[ScenarioRunEvent[S,  E]], Try[Either[List[Fail[E]], S]]) = {
    val src = flow.source
    val steps = flow.steps

    val sourceSubj = SourceSubject(flow.sourceDescription, flow.filePosition)
    cron(Try(src())) match {
      case (start, end, m @ Failure(err)) => List[ScenarioRunEvent[S,  E]](SourceErrorRunEvent(sourceSubj, err, start, end)) -> Failure(err)
      case (start, end, m @ Success(v)) =>
        val evs = List[ScenarioRunEvent[S,  E]](SourceSuccessRunEvent[S,  E](sourceSubj, v, start, end))
        val z = evs -> m.map { s =>
          Right(s):Either[List[Fail[E]], S]
        }
        steps.foldLeft(z) { (acc, step) =>
          val (evs, tm) = acc
          tm match {
            case Failure(err) => acc
            case Success(Left(errs)) => acc
            case Success(Right(ms)) =>
              val (evs1, res) = step.fold(
                  onStep[S,E](step, ms), 
                  onExpectation[S,E](step, ms))
              (evs ++ evs1) -> res
          }
        }
    }
  }

}