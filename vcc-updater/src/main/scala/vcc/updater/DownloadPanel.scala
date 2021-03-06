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

import scala.swing._
import scala.swing.event._
import java.net.URL
import java.io.File
import vcc.util.swing.MigPanel
import vcc.util.swing.multipanel.AbstractPanel
import akka.actor.{ActorSystem, ActorDSL}
import akka.actor.ActorDSL._
import scala.concurrent.SyncVar

class DownloadPanel(url: URL, targetFile: File) extends MigPanel("") with AbstractPanel[Option[File]] {

  private val logger = org.slf4j.LoggerFactory.getLogger("infra")

  add(new Label("Downloading: " + url), "wrap")
  private val progress = new ProgressBar()
  private val cancelButton = new Button("Cancel")
  private val kBytesCount = new Label("0/0")

  private var cancellationTrigger: SyncVar[Boolean] = null
  add(progress, "wrap,align center")
  add(kBytesCount, "wrap,align center")
  add(cancelButton, "wrap,align center")

  listenTo(cancelButton)

  private val system = ActorSystem("migration-system")
  private val observer = ActorDSL.actor(system)(new ActWithStash {
    override def receive = {
      case Downloader.Progress(current, total) if current == total =>
        finish('COMPLETE)
        notifyController(Some(targetFile))
      case Downloader.Progress(current, total) =>
        progress.value = (100.0 * (current.toDouble / total.toDouble)).toInt
        kBytesCount.text = current + "/" + total + " bytes"
      case Downloader.Cancel() =>
        finish('CANCEL)
      case Downloader.DownloadActor(actor) =>
        cancellationTrigger = actor
      case Downloader.Failed(msg) =>
        logger.error("Failed download: " + msg)
        finish(msg)
      case s =>
        logger.warn("DownloadPanel unhandled event: " + s)
    }

    private def finish(msg: Any) {
      if (msg == 'COMPLETE)
        notifyController(Some(targetFile))
      else
        notifyController(None)
      context.stop(self)
    }
  })

  val downloader = new Downloader(url, targetFile)

  reactions += {
    case b: ButtonClicked =>
      cancellationTrigger.put(true)
  }

  //Only Start download when I have the actor set
  override def setRemote(actor: SyncVar[Option[File]]) = {
    super.setRemote(actor)
    downloader.start(observer)
  }
}