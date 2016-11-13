package com.github.gdefacci.bdd
package runner

import sbt.testing._

class BaseSbtEvent(task: TaskDef, testName: String, val status: Status, val throwable: OptionalThrowable, val duration: Long) extends Event {

  def fullyQualifiedName(): String =
    task.fullyQualifiedName()

  def selector(): Selector =
    new TestSelector(testName)

  def fingerprint(): Fingerprint =
    task.fingerprint()

}

class SbtEvents(task: TaskDef) {

  def success(testName: String, durationMillis: Long): Event =
    new BaseSbtEvent(task, testName, Status.Success, new OptionalThrowable(), durationMillis)
  //    new Event {
  //    def fullyQualifiedName(): String =
  //      task.fullyQualifiedName()
  //
  //    def throwable(): OptionalThrowable =
  //      new OptionalThrowable()
  //
  //    def status(): Status = Status.Success
  //
  //    def selector(): Selector =
  //      new TestSelector(testName)
  //
  //    def fingerprint(): Fingerprint =
  //      task.fingerprint()
  //
  //    def duration(): Long =
  //      durationMillis
  //  }

  def error(testName: String, result: ErrorEvent): Event =
    new BaseSbtEvent(task, testName, result match {
      case errEv: ExceptionEvent => Status.Error
      case _ => Status.Failure
    }, result match {
      case errEv: ExceptionEvent => new OptionalThrowable(errEv.exception)
      case failure: ExpectationFailure => new OptionalThrowable(new RuntimeException(failure.subject.toString))
    }, -1)
  //    new Event {
  //    
  //    def fullyQualifiedName(): String =
  //      task.fullyQualifiedName()
  //
  //    def throwable(): OptionalThrowable =
  //      result match {
  //        case errEv: ExceptionEvent => new OptionalThrowable(errEv.exception)
  //        case failure: ExpectationFailure => new OptionalThrowable(new RuntimeException(failure.subject.toString))
  //      }
  //
  //    def status(): Status =
  //      result match {
  //        case errEv: ExceptionEvent => Status.Error
  //        case _ => Status.Failure
  //      }
  //
  //    def selector(): Selector =
  //      task.selectors().head
  //
  //    def fingerprint(): Fingerprint =
  //      task.fingerprint()
  //
  //    def duration(): Long =
  //      0
  //  }

  def error(testName:String, err: Throwable): Event = 
     new BaseSbtEvent(task, testName,  Status.Error, new OptionalThrowable(err), -1)
    
//    new Event {
//    def fullyQualifiedName(): String =
//      task.fullyQualifiedName()
//
//    def throwable(): OptionalThrowable =
//      new OptionalThrowable(err)
//
//    def status(): Status =
//      Status.Error
//
//    def selector(): Selector =
//      task.selectors().head
//
//    def fingerprint(): Fingerprint =
//      task.fingerprint()
//
//    def duration(): Long =
//      0
//  }
}