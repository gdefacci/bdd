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

  def cron[T](action: => Try[T]):(Long, Long, Try[T]) = {
    val st = System.currentTimeMillis()
    val r = action
    val end = System.currentTimeMillis()
    (st, end, r)
  }
  
  def onStep[S,E](step:FlowStep[S,M,E], ms:M[S]):(S => M[S]) => (List[ScenarioRunEvent[S, M, E]], Try[Either[List[Fail[E]], M[S]]]) = { run =>
    val stepSubj = StepSubject(step.description, step.filePosition)

    cron(Try(ms.flatMap(run))) match {
      case (start, end, rm @ Success(v)) =>
        (Nil :+ StepSuccessRunEvent[S, M, E](stepSubj, ms, v, start, end)) -> rm.map(Right(_))
      case (start, end, rm @ Failure(err)) =>
        (Nil :+ StepErrorRunEvent[S, M, E](stepSubj, ms, err, start, end)) -> rm.map(Right(_))
    }
  }
  
  def onExpectation[S,E](exp:FlowStep[S,M,E], ms:M[S]):(M[S] => Seq[TestResult[E]]) => (List[ScenarioRunEvent[S, M, E]], Try[Either[List[Fail[E]], M[S]]]) = { predicate =>
    val subj = ExpectationSubject(exp.description, exp.filePosition)
    cron(Try(predicate(ms))) match {
      case (start, end, Success(trs)) if trs.forall(_ == Ok) =>
        List(ExpectationSuccessRunEvent[S, M, E](subj, ms, start, end)) -> Success(Right(ms))
      case (start, end, Success(trs)) =>
        val failures = trs.collect { case f @ Fail(e) => e }.toList
        List(ExpectationFailureRunEvent[S, M, E](subj, ms, failures, start, end)) -> Success(Left(failures.map(Fail[E](_))))
      case (start, end, Failure(err)) =>
        List(ExpectationErrorRunEvent[S, M, E](subj, ms, err, start, end)) -> Failure(err)
    }
  }

  def runEvents[S, E](flow: Flow[S, M, E]):List[ScenarioRunEvent[S, M, E]] = {
    run[S,E](flow)._1
  }
  
  def run[S, E](flow: Flow[S, M, E]):(List[ScenarioRunEvent[S, M, E]], Try[Either[List[Fail[E]], M[S]]]) = {
    val src = flow.source
    val steps = flow.steps

    val sourceSubj = SourceSubject(flow.sourceDescription, flow.filePosition)
    cron(Try(src())) match {
      case (start, end, m @ Failure(err)) => List[ScenarioRunEvent[S, M, E]](SourceErrorRunEvent(sourceSubj, err, start, end)) -> Failure(err)
      case (start, end, m @ Success(v)) =>
        val evs = List[ScenarioRunEvent[S, M, E]](SourceSuccessRunEvent[S, M, E](sourceSubj, v, start, end))
        val z = evs -> m.map { s =>
          val r = Right(Monad[M].pure(s))
          r:Either[List[Fail[E]], M[S]]
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