package com.github.gdefacci.bdd
package runner

import sbt.testing._

class SbtEvents(task: TaskDef) {

  def success(testName:String, durationMillis: Long): Event = new Event {
    def fullyQualifiedName(): String =
      task.fullyQualifiedName()

    def throwable(): OptionalThrowable = 
      new OptionalThrowable()

    def status(): Status = Status.Success

    def selector(): Selector = 
      new TestSelector(testName)

    def fingerprint(): Fingerprint =
      task.fingerprint()

    def duration(): Long =
      durationMillis
  }

  def error(result: ErrorEvent): Event = new Event {
    def fullyQualifiedName(): String =
      task.fullyQualifiedName()

    def throwable(): OptionalThrowable =
      result match {
        case errEv: ExceptionEvent => new OptionalThrowable(errEv.exception)
        case failure: ExpectationFailure => new OptionalThrowable(new RuntimeException(failure.subject.toString))
      }

    def status(): Status =
      result match {
        case errEv: ExceptionEvent => Status.Error
        case _ => Status.Failure
      }

    def selector(): Selector = 
      task.selectors().head

    def fingerprint(): Fingerprint =
      task.fingerprint()

    def duration(): Long =
      0
  }
  
  def error(err: Throwable): Event = new Event {
    def fullyQualifiedName(): String =
      task.fullyQualifiedName()

    def throwable(): OptionalThrowable =
         new OptionalThrowable(err)

    def status(): Status =
     Status.Error

    def selector(): Selector = 
      task.selectors().head

    def fingerprint(): Fingerprint =
      task.fingerprint()

    def duration(): Long =
      0
  }
}