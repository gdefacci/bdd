package com.github.gdefacci.bdd
package runner

import sbt.testing.{Runner => BaseRunner, Task => BaseTask, _}

final class Runner(
  val args: Array[String],
  val remoteArgs: Array[String],
  classLoader: ClassLoader)
  extends BaseRunner {

  def done(): String = ""

  def tasks(list: Array[TaskDef]): Array[BaseTask] = {
    list.map(t => new Task(t, classLoader))
  }

}
