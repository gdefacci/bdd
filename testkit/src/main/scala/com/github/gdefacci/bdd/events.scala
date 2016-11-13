package com.github.gdefacci.bdd

sealed trait RunEvent {
  def startTime: Long
  def endTime: Long
  def subject: EventSubject
  
  def totalTime = endTime - startTime
}

sealed trait ScenarioRunEvent[S, E] extends RunEvent 

sealed trait EventSubject {
  def description:Description
  def position:Option[FilePosition]
}

sealed trait StepLikeSubject extends EventSubject
case class SourceSubject(description:Description, position:Option[FilePosition]) extends EventSubject
case class StepSubject(description:Description, position:Option[FilePosition]) extends StepLikeSubject
case class ExpectationSubject(description:Description, position:Option[FilePosition]) extends EventSubject

sealed trait SuccessEvent extends RunEvent
sealed trait SuccessRunEvent[S,  E]  extends SuccessEvent with ScenarioRunEvent[S,E] 

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

case class SourceSuccessRunEvent[S,  E](subject: SourceSubject, output:S, startTime:Long, endTime: Long) extends SourceSuccess with SuccessRunEvent[S,E]
case class StepSuccessRunEvent[S,  E](subject: StepSubject, input:S, output:S, startTime:Long, endTime: Long) extends StepSuccess with SuccessRunEvent[S,E]
case class ExpectationSuccessRunEvent[S, E](subject: ExpectationSubject, input:S, startTime:Long, endTime: Long) extends ExpectationSuccess with SuccessRunEvent[S,E]

sealed trait ErrorEvent extends RunEvent
sealed trait ErrorRunEvent[S, E] extends ErrorEvent with ScenarioRunEvent[S,E]

sealed trait ExceptionEvent extends ErrorEvent {
  def exception: Throwable
}
sealed trait ExceptionRunEvent[S, E] extends ExceptionEvent with ErrorRunEvent[S,E] 

object ExceptionRunEvent {

  def unapply[S, E](ev: ExceptionRunEvent[S,E]): Option[(EventSubject, Throwable)] = {
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

case class SourceErrorRunEvent[S, E](subject: SourceSubject, exception: Throwable, startTime:Long, endTime: Long) extends SourceError with ExceptionRunEvent[S,E]
case class StepErrorRunEvent[S,  E](subject: StepSubject, input: S, exception: Throwable, startTime:Long, endTime: Long) extends StepError with ExceptionRunEvent[S,E]
case class ExpectationErrorRunEvent[S,  E](subject: ExpectationSubject, input: S, exception: Throwable, startTime:Long, endTime: Long) extends ExpectationError with ExceptionRunEvent[S,E]
case class ExpectationFailureRunEvent[S,  E](subject: ExpectationSubject, input: S, failures: Seq[E], startTime:Long, endTime: Long) extends ExpectationFailure with ErrorRunEvent[S,E]