package com.github.gdefacci.bdd

import language.higherKinds

sealed trait RunEvent {
  def startTime: Long
  def endTime: Long
  def subject: EventSubject
  
  def totalTime = endTime - startTime
}

sealed trait ScenarioRunEvent[S, M[_], E] extends RunEvent 

sealed trait EventSubject {
  def description:Description
  def position:Option[FilePosition]
}

sealed trait StepLikeSubject extends EventSubject
case class SourceSubject(description:Description, position:Option[FilePosition]) extends EventSubject
case class StepSubject(description:Description, position:Option[FilePosition]) extends StepLikeSubject
case class ExpectationSubject(description:Description, position:Option[FilePosition]) extends EventSubject

sealed trait SuccessEvent extends RunEvent
sealed trait SuccessRunEvent[S, M[_], E]  extends SuccessEvent with ScenarioRunEvent[S,M,E] 

sealed trait SourceSuccess extends SuccessEvent {
  def output:Any
}

sealed trait StepSuccess extends SuccessEvent {
  def input:Any
  def output:Any
}

sealed trait ExpectationSuccess extends SuccessEvent {
  def input:Any
}

case class SourceSuccessRunEvent[S, M[_], E](subject: SourceSubject, output:S, startTime:Long, endTime: Long) extends SourceSuccess with SuccessRunEvent[S,M,E]
case class StepSuccessRunEvent[S, M[_], E](subject: StepSubject, input:M[S], output:M[S], startTime:Long, endTime: Long) extends StepSuccess with SuccessRunEvent[S,M,E]
case class ExpectationSuccessRunEvent[S, M[_], E](subject: ExpectationSubject, input:M[S], startTime:Long, endTime: Long) extends ExpectationSuccess with SuccessRunEvent[S,M,E]

sealed trait ErrorEvent extends RunEvent
sealed trait ErrorRunEvent[S, M[_], E] extends ErrorEvent with ScenarioRunEvent[S,M,E]

sealed trait ExceptionEvent extends ErrorEvent {
  def exception: Throwable
}
sealed trait ExceptionRunEvent[S, M[_], E] extends ExceptionEvent with ErrorRunEvent[S,M,E] 

object ExceptionRunEvent {

  def unapply[S, M[_], E](ev: ExceptionRunEvent[S,M,E]): Option[(EventSubject, Throwable)] = {
    Some(ev.subject -> ev.exception)
  }
}

sealed trait SourceError extends ExceptionEvent 

sealed trait StepError extends ExceptionEvent {
  def input:Any
}

sealed trait ExpectationError extends ExceptionEvent {
  def input:Any
}

sealed trait ExpectationFailure extends ErrorEvent {
  def input:Any
  def failures: Seq[Any]
}

case class SourceErrorRunEvent[S, M[_], E](subject: SourceSubject, exception: Throwable, startTime:Long, endTime: Long) extends SourceError with ExceptionRunEvent[S,M,E]
case class StepErrorRunEvent[S,  M[_], E](subject: StepSubject, input: M[S], exception: Throwable, startTime:Long, endTime: Long) extends StepError with ExceptionRunEvent[S,M,E]
case class ExpectationErrorRunEvent[S,  M[_], E](subject: ExpectationSubject, input: M[S], exception: Throwable, startTime:Long, endTime: Long) extends ExpectationError with ExceptionRunEvent[S,M,E]
case class ExpectationFailureRunEvent[S,  M[_], E](subject: ExpectationSubject, input: M[S], failures: Seq[E], startTime:Long, endTime: Long) extends ExpectationFailure with ErrorRunEvent[S,M,E]