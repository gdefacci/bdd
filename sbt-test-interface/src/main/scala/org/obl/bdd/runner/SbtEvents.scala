package org.obl.bdd
package runner

import sbt.testing._

class SbtEvents(task: TaskDef) {

  def success(result: SuccessfullScenario[_, _], durationMillis: Long): Event = new Event {
    def fullyQualifiedName(): String =
      task.fullyQualifiedName()

    def throwable(): OptionalThrowable = 
      new OptionalThrowable()

    def status(): Status = Status.Success

    def selector(): Selector = 
      task.selectors().head

    def fingerprint(): Fingerprint =
      task.fingerprint()

    def duration(): Long =
      durationMillis
  }

  def error(result: ErrorEvent[_, _]): Event = new Event {
    def fullyQualifiedName(): String =
      task.fullyQualifiedName()

    def throwable(): OptionalThrowable =
      result match {
        case errEv: ExceptionEvent[_, _] => new OptionalThrowable(errEv.exception)
        case failure: ExpectationFailure[_, _] => new OptionalThrowable(new RuntimeException(failure.failure.toString))
      }

    def status(): Status =
      result match {
        case errEv: ExceptionEvent[_, _] => Status.Error
        case _ => Status.Failure
      }

    def selector(): Selector = 
      task.selectors().head

    def fingerprint(): Fingerprint =
      task.fingerprint()

    def duration(): Long =
      0
  }
}

/**
class SbtEvents(task: TaskDef) {

  def success(result: SuccessfullScenario[_, _], durationMillis: Long): Event = new Event {
    def fullyQualifiedName(): String =
      task.fullyQualifiedName()

    def throwable(): OptionalThrowable =
      new OptionalThrowable()

    def status(): Status = Status.Success

    def selector(): Selector = {
      task.selectors().head
    }

    def fingerprint(): Fingerprint =
      task.fingerprint()

    def duration(): Long =
      durationMillis
  }

  def error(result: ErrorEvent[_, _]): Event = new Event {
    def fullyQualifiedName(): String =
      task.fullyQualifiedName()

    def throwable(): OptionalThrowable =
      result match {
        case errEv: ExceptionEvent[_, _] => new OptionalThrowable(errEv.exception)
        case failure: ExpectationFailure[_, _] => new OptionalThrowable(new RuntimeException(failure.failure.toString))
      }

    def status(): Status =
      result match {
        case errEv: ExceptionEvent[_, _] => Status.Error
        case _ => Status.Failure
      }

    def selector(): Selector = {
      task.selectors().head
    }

    def fingerprint(): Fingerprint =
      task.fingerprint()

    def duration(): Long =
      0
  }
}
 */
