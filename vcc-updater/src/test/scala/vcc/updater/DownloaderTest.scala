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
package vcc.updater

import java.net.URL
import scala.actors.{TIMEOUT, Actor}
import org.junit.{Assert, Test, Ignore}
import scala.actors.migration.{ActWithStash, ActorDSL}
import java.io.File
import scala.concurrent.duration._

class DownloaderTest {

  //TODO This must be replace by proper Akka testing
  def peerActor(parent: Actor, f: PartialFunction[Any, Boolean]) = ActorDSL.actor(new ActWithStash {

    var running = true
    var trouble: Exception = null

    override def preStart() {
      context.setReceiveTimeout(10000 millisecond)
    }

    def receive = {
      case ReceiveTimeout =>
        parent ! TIMEOUT
      case msg if f.isDefinedAt(msg) =>
        try {
          running = f(msg)
          if(!running) {
            parent ! 'DONE
            context.stop(self)
          }
        } catch {
          case e:Exception =>
            parent ! e
            context.stop(self)
        }
    }
  })

  private val testActorBehaviour: PartialFunction[Any, Unit] = {
    case 'DONE => Assert.assertTrue(true)
    case e: Exception => Assert.fail("Failed with exception" + e)
  }

  def executeIfForReal(block: => Unit) {
    if (System.getProperty("vcc.test.download") != null || true)
      block
    else
      Assert.assertTrue(true)
  }

  @Test
  def testDownloadBadURL() {
    executeIfForReal {
      val file = File.createTempFile("vcc", ".zip")
      val down = new Downloader(new URL("http://127.0.0.1/a.zip"), file)
      down.start(peerActor(Actor.self, {
        case Downloader.Failed(msg) =>
          false // End run correctly
        case Downloader.DownloadActor(actor) =>
          true
        case TIMEOUT =>
          throw new Exception("Timeout, should not get here")
          false
        case s =>
          println("Got :: " + s)
          throw new Exception("This should not be reached")
          false
      }))

      Actor.receive(testActorBehaviour)
      file.delete()
    }
  }

  @Test
  def testDownloadingXML() {
    executeIfForReal {
      val file = File.createTempFile("vcc", ".xml")
      val downloader = new Downloader(new URL("http://www.exnebula.org/files/release-history/vcc/vcc-all.xml"), file)
      downloader.start(peerActor(Actor.self, {
        case scala.actors.TIMEOUT =>
          throw new Exception("Timeout, should not get here")
          false
        case Downloader.Failed(msg) =>
          throw new Exception("Got a message: " + msg)
          false
        case Downloader.DownloadActor(actor) =>
          true
        case Downloader.Progress(down, total) if down == total =>
          if (down == total) println("Completed!")
          false
        case Downloader.Progress(down, total) =>
          println("Progress :" + down + "/" + total)
          true
        case s =>
          println("Got : " + s)
          throw new Exception("This should not be reached" + s)
      }))
      Actor.receive(testActorBehaviour)
      if (file.exists()) file.delete()
    }
  }

  @Ignore()
  def testWithActualZipFile() {
    executeIfForReal {
      val file = File.createTempFile("vcc", ".zip")
      val downloader = new Downloader(new URL("http://www.exnebula.org/files/vcc/vcc-0.10.zip"), file)
      downloader.start(peerActor(Actor.self, {
        case scala.actors.TIMEOUT =>
          throw new Exception("Timeout, should not get here")
          false
        case Downloader.Failed(msg) =>
          throw new Exception("Got a message " + msg)
          false
        case Downloader.DownloadActor(actor) =>
          true
        case Downloader.Progress(down, total) if down == total =>
          if (down == total) println("Completed!")
          false
        case Downloader.Progress(down, total) =>
          println(Thread.currentThread + "Progress :" + down + "/" + total)
          true
        case s =>
          println("Got : " + s)
          throw new Exception("This should not be reached" + s)
      }))
      Actor.receive(testActorBehaviour)
      if (file.exists()) file.delete()
    }
  }

  @Test
  def testCancelMidDownload() {
    executeIfForReal {
      val file = File.createTempFile("vcc", ".xml")
      val downloader = new Downloader(new URL("http://www.exnebula.org/files/release-history/vcc/vcc-all.xml"), file)

      downloader.start(peerActor(Actor.self, {
        case scala.actors.TIMEOUT =>
          throw new Exception("Timeout, should not get here")
          false
        case Downloader.Failed(msg) =>
          throw new Exception("Got a message " + msg)
          false
        case Downloader.DownloadActor(actor) =>
          val dActor = actor
          Thread.sleep(50)
          dActor ! Downloader.Cancel()
          true
        case Downloader.Progress(down, total) if down == total =>
          throw new Exception("Should have cancelled")
          false
        case Downloader.Progress(down, total) =>
          println(Thread.currentThread + "Progress :" + down + "/" + total)
          true
        case Downloader.Cancel() =>
          false
        case s =>
          println("Got : " + s)
          throw new Exception("This should not be reached" + s)
      }))
      Actor.receive(testActorBehaviour)
      if (file.exists()) file.delete()
    }
  }

}