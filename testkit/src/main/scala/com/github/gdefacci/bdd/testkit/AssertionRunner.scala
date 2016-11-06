package com.github.gdefacci.bdd
package testkit

import language.higherKinds
import scalaz.Monad
import scala.util.Try
import scalaz.ListT
import scalaz.Traverse
import scala.util.Failure
import scalaz.Applicative
import scala.util.Success

class AssertionRunner[M[_]: Monad] {

  import scalaz.syntax.monad._
  import scalaz.syntax.traverse._
  import scalaz.std.list._

  def run[S, E](assertion: Assertion[S, M, E]): Try[List[(Description, TestResult[E])]] = {
    val flow = assertion.flow
    val src = flow.source
    val steps = flow.steps

    val r1 = steps.foldLeft(Try(Monad[M].pure(src()))) { (tm, step) =>
      tm.flatMap(m => Try(m.flatMap(step.run)))
    }
    r1.flatMap { res =>
      assertion.expectations.toList.flatMap(exp => Try(exp.predicate(res).map(exp.description -> _).toList).sequence).sequence
    }
  }

  def cron[T](action: => Try[T]):(Long, Long, Try[T]) = {
    val st = System.currentTimeMillis()
    val r = action
    val end = System.currentTimeMillis()
    (st, end, r)
  }

  def runEvents[S, E](assertion: Assertion[S, M, E]):List[ScenarioRunEvent[S, M, E]] = {
    val flow = assertion.flow
    val src = flow.source
    val steps = flow.steps

    val sourceSubj = SourceSubject(flow.sourceDescription, flow.filePosition)
    val (stepEvents, res1: Try[M[S]]) = cron(Try(src())) match {
      case (start, end, m @ Failure(err)) => List[ScenarioRunEvent[S, M, E]](SourceErrorRunEvent(sourceSubj, err, start, end)) -> Failure(err)
      case (start, end, m @ Success(v)) =>
        val evs = List[ScenarioRunEvent[S, M, E]](SourceSuccessRunEvent[S, M, E](sourceSubj, v, start, end))
        steps.foldLeft(evs -> m.map(Monad[M].pure(_))) { (acc, step) =>
          val (evs, tm) = acc
          tm match {
            case Failure(err) => acc
            case Success(m) =>
              val stepSubj = StepSubject(step.description, step.filePosition)

              cron(Try(m.flatMap(step.run))) match {
                case (start, end, rm @ Success(v)) =>
                  (evs :+ StepSuccessRunEvent[S, M, E](stepSubj, m, v, start, end)) -> rm
                case (start, end, rm @ Failure(err)) =>
                  (evs :+ StepErrorRunEvent[S, M, E](stepSubj, m, err, start, end)) -> rm
              }
          }
        }
    }
    res1 match {
      case Failure(err) => stepEvents
      case Success(res) =>
        stepEvents ++ assertion.expectations.toList.map { exp =>
          val subj = ExpectationSubject(exp.description, exp.filePosition)
          cron(Try(exp.predicate(res))) match {
            case (start, end, Success(trs)) if trs.forall(_ == Ok) =>
              ExpectationSuccessRunEvent[S, M, E](subj, res, start, end)
            case (start, end, Success(trs)) =>
              ExpectationFailureRunEvent[S, M, E](subj, res, trs.collect { case Fail(e) => e }, start, end)
            case (start, end, Failure(err)) =>
              ExpectationErrorRunEvent[S, M, E](subj, res, err, start, end)
          }
        }
    }
  }

}