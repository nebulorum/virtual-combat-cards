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
import org.junit.{Assert, Test, Ignore}
import akka.actor.{ActorSystem, ReceiveTimeout, ActorDSL}
import akka.actor.ActorDSL._
import java.io.File
import scala.concurrent.duration._
import scala.concurrent.{TimeoutException, SyncVar}
import scala.language.postfixOps

class DownloaderTest {

  private val system = ActorSystem("migration-system")

  private def peerActor(parent: SyncVar[Option[Throwable]], f: PartialFunction[Any, Boolean]) = ActorDSL.actor(system)(new ActWithStash {

    var running = true
    var trouble: Exception = null

    override def preStart() {
      context.setReceiveTimeout(10000 millisecond)
    }

    override def receive = {
      case ReceiveTimeout =>
        parent.put(Some(new TimeoutException()))
      case msg if f.isDefinedAt(msg) =>
        try {
          running = f(msg)
          if (!running) {
            parent.put(None)
            context.stop(self)
          }
        } catch {
          case e: Exception =>
            parent.put(Some(e))
            context.stop(self)
        }
    }
  })

  private val testActorBehaviour: PartialFunction[Option[Throwable], Unit] = {
    case None => Assert.assertTrue(true)
    case Some(e) => Assert.fail("Failed with exception" + e)
  }

  private def executeIfForReal(block: => Unit) {
    if (System.getProperty("vcc.test.download") != null)
      block
    else
      Assert.assertTrue(true)
  }

  @Test
  def testDownloadBadURL() {
    executeIfForReal {
      val barrier = new SyncVar[Option[Throwable]]
      val file = File.createTempFile("vcc", ".zip")
      val down = new Downloader(new URL("http://127.0.0.1/a.zip"), file)
      down.start(peerActor(barrier, {
        case Downloader.Failed(msg) =>
          false // End run correctly
        case Downloader.DownloadActor(actor) =>
          true
        case s =>
          println("Got :: " + s)
          throw new Exception("This should not be reached")
          false
      }))
      testActorBehaviour(barrier.get)
      file.delete()
    }
  }

  @Test
  def testDownloadingXML() {
    executeIfForReal {
      val barrier = new SyncVar[Option[Throwable]]
      val file = File.createTempFile("vcc", ".xml")
      val downloader = new Downloader(new URL("http://www.exnebula.org/files/release-history/vcc/vcc-all.xml"), file)
      downloader.start(peerActor(barrier, {
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
      testActorBehaviour(barrier.get)
      if (file.exists()) file.delete()
    }
  }

  @Ignore()
  def testWithActualZipFile() {
    executeIfForReal {
      val barrier = new SyncVar[Option[Throwable]]
      val file = File.createTempFile("vcc", ".zip")
      val downloader = new Downloader(new URL("http://www.exnebula.org/files/vcc/vcc-0.10.zip"), file)
      downloader.start(peerActor(barrier, {
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
      testActorBehaviour(barrier.get)
      if (file.exists()) file.delete()
    }
  }

  @Test
  def testCancelMidDownload() {
    executeIfForReal {
      val barrier = new SyncVar[Option[Throwable]]
      val file = File.createTempFile("vcc", ".xml")
      val downloader = new Downloader(new URL("http://www.exnebula.org/files/release-history/vcc/vcc-all.xml"), file)

      downloader.start(peerActor(barrier, {
        case Downloader.Failed(msg) =>
          throw new Exception("Got a message " + msg)
          false
        case Downloader.DownloadActor(actor) =>
          val dActor = actor
          Thread.sleep(50)
          dActor.put(true)
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
      testActorBehaviour(barrier.get)
      if (file.exists()) file.delete()
    }
  }
}