/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.application

import org.specs2.SpecificationWithJUnit
import concurrent.SyncVar
import org.specs2.specification.AroundExample
import org.specs2.time.TimeConversions
import org.specs2.execute.{AsResult, Result}
import org.specs2.execute.EventuallyResults._
import scala.actors.migration.{ActWithStash, ActorDSL}

trait RetryExamples extends AroundExample {

  import TimeConversions._

  protected def around[T](t: => T)(implicit evidence$1: AsResult[T]): Result =
    eventually(retries = 3, sleep = 10.millis)(evidence$1.asResult(t))
}

class LongPollTackerObserverTest extends SpecificationWithJUnit with RetryExamples {

  import scala.concurrent.duration._

  def is = "LongPollTackerObserver".title ^
    "send in time" ! s().e1 ^
    "timeout on wait" ! s().e2 ^
    "one early one late" ! s().e3 ^
    "update n" ! s().e4 ^
    "interleave update with setting" ! s().interleaveUpdateAndSubscribe ^
    end

  case class s() {
    private val obs = new LongPollTrackerObserver[Int]()

    def e1 = {
      val f = new SyncVar[Int]
      obs.waitingForState(f)
      updateObserverIn(100 milli, 10)
      f.get(150) must_== Some(10)
    }

    def e2 = {
      val f = new SyncVar[Int]
      obs.waitingForState(f)
      updateObserverIn(100 milli, 10)
      f.get(50) must_== None
    }

    def e3 = {
      val f = new SyncVar[Int]
      val f2 = new SyncVar[Int]
      obs.waitingForState(f2)
      obs.waitingForState(f)
      updateObserverIn(100 milli, 10)
      val v1 = f.get(60)
      val v2 = f2.get(60)
      (v1 must_== None) and (v2 must_== Some(10))
    }

    def e4 = {
      val svs = (0 to 4).toSeq map (_ => new SyncVar[Int]())
      svs.foreach(obs.waitingForState)
      updateObserverIn(100 milli, 12)
      Thread.sleep(120)

      svs.map(_.get) must_== Seq.fill(5)(12)
    }

    def interleaveUpdateAndSubscribe = {
      val f = new SyncVar[Int]
      val f2 = new SyncVar[Int]
      obs.waitingForState(f)

      executeBlockIn(40 milli) {
        obs.stateUpdated(13)
        obs.waitingForState(f2)
      }
      (f.get(60) must_== Some(13)) and (f2.get(60) must_== None)
    }

    private def updateObserverIn(sleep: Duration, value: Int) = executeBlockIn(sleep) { obs.stateUpdated(value) }

    private def executeBlockIn(sleep: Duration)(block: => Unit) {
      ActorDSL.actor(new ActWithStash {
        context.setReceiveTimeout(sleep)
        def receive = {
          case ReceiveTimeout =>
            block
            context.stop(self)
        }
      })
    }
  }

}