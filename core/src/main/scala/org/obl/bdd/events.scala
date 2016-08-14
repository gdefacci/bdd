package org.obl.bdd

sealed trait ScenarioRunEvent[S, E]{
  def time:Long
  def subject:EventSubject[S, E]
}

sealed trait EventSubject[S, E]

sealed trait StepLikeSubject[S, E] extends EventSubject[S, E]
case class SourceSubject[S, E](source: Source[S]) extends EventSubject[S, E]
case class StepSubject[S, E](step: Step[S]) extends StepLikeSubject[S, E]
case class ActionSubject[S, E](action: Action[S]) extends StepLikeSubject[S, E]
case class ExpectationSubject[S, E](expectation: Expectation[S, E]) extends EventSubject[S, E]

case class StartEvent[S, E](subject: EventSubject[S, E], time: Long) extends ScenarioRunEvent[S, E]
case class SuccessEvent[S, E](subject: EventSubject[S, E], output: S, time: Long) extends ScenarioRunEvent[S, E]

sealed trait ErrorEvent[S,E] extends ScenarioRunEvent[S, E]

sealed trait ExceptionEvent[S,E] extends ErrorEvent[S,E] {
  def exception: Throwable
} 

object ExceptionEvent {
  
  def getInput[S,E](err:ExceptionEvent[S,E]):Option[S] = err match {
    case SourceError(_,_,_) => None
    case StepError(_,input,_,_) => Some(input)
    case ActionError(_,input,_,_) => Some(input)
    case ExpectationError(_,input,_,_) => Some(input)
  }
  
  def unapply[S,E](ev:ExceptionEvent[S,E]):Option[(EventSubject[S,E], Throwable)] = {
    Some(ev.subject -> ev.exception)
  }
} 

case class SourceError[S, E](subject: SourceSubject[S,E], time: Long, exception: Throwable) extends ExceptionEvent[S, E]
case class StepError[S, E](subject: StepSubject[S,E], stepInput: S, time: Long, exception: Throwable) extends ExceptionEvent[S, E]
case class ActionError[S, E](subject: ActionSubject[S,E], actionInput: S, time: Long, exception: Throwable) extends ExceptionEvent[S, E]
case class ExpectationError[S, E](subject: ExpectationSubject[S, E], input: S, time: Long, exception: Throwable) extends ExceptionEvent[S, E]
case class ExpectationFailure[S, E](subject: ExpectationSubject[S, E], input: S, failure: E, time: Long) extends ErrorEvent[S, E]
