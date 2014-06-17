/*
 * Copyright (C) 2008-2014 - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
package vcc.util

import akka.actor.ActorDSL._
import akka.actor.{ActorSystem, ActorDSL}

trait AsynchronousTask[T] {
  def execute(): T
}

object AsynchronousDispatcher {

  trait Observer[T] {
    def taskComplete(task: AsynchronousTask[T], result: T)

    def taskFailed(task: AsynchronousTask[T], error: Throwable)

  }

}

class AsynchronousDispatcher[T]() {
  private val system = ActorSystem("migration-system")
  private var observer: AsynchronousDispatcher.Observer[T] = null
  private val worker = ActorDSL.actor(system)(new ActWithStash {
    override def receive = {
      case task: AsynchronousTask[T] =>
        try {
          observer.taskComplete(task, task.execute())
        } catch {
          case s: Exception =>
            observer.taskFailed(task, s)
        }
    }
  })

  private case class Complete(task: AsynchronousTask[T])

  def setObserver(observer: AsynchronousDispatcher.Observer[T]) {
    this.observer = observer
  }

  def queueTasks(tasks: Seq[AsynchronousTask[T]]) {
    if (observer == null)
      throw new IllegalStateException("Observer not set")
    tasks.foreach {
      task => worker ! task
    }
  }

}