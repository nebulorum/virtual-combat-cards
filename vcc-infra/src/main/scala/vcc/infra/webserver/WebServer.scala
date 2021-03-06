/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.infra.webserver

import org.mortbay.jetty.Server
import org.mortbay.jetty.bio.SocketConnector
import org.mortbay.component._
import org.mortbay.jetty.webapp.WebAppContext

class WebServer(port: Int, webDir: String) {

  private val logger = org.slf4j.LoggerFactory.getLogger("infra")

  protected object ServerStatus extends LifeCycle.Listener {
    var running: Boolean = false

    def lifeCycleFailure(event: LifeCycle, cause: Throwable) {
      logger.info("Server Failed")
    }

    def lifeCycleStarted(event: LifeCycle) {
      logger.info("Server started")
      running = true
    }

    def lifeCycleStarting(event: LifeCycle) {
      logger.info("Server starting")
    }

    def lifeCycleStopped(event: LifeCycle) {
      logger.info("Server stopped")
      running = false
    }

    def lifeCycleStopping(event: LifeCycle) {
      logger.info("Server stoping")
    }
  }

  private val server = new Server()
  private val warContext = new WebAppContext()

  protected def initialize(webContentURL: String) {
    val connector = new SocketConnector()
    connector.setPort(port)
    server.setConnectors(Array(connector))
    server.addLifeCycleListener(ServerStatus)

    warContext.setWar(webContentURL)
    warContext.setContextPath("/")
    server.addHandler(warContext)
  }

  initialize(webDir)

  def running = ServerStatus.running

  def start() {
    try {
      server.start()
    } catch {
      case _: Throwable =>
        server.stop()
        server.join()
    }
  }

  def stop() {
    server.stop()
    server.join()
  }

  def registerServlet(classPath: String, path: String) {
    warContext.addServlet(classPath, path)
  }

}

object WebServer {

  /**
   * Create a WebServer
   * @param webContentURL String of URL to web content
   * @param port Port to run server on
   * @param servletMap A map for path to Servlet class that is used to register servlet that handle each path
   */
  def initialize(webContentURL: String, port: Int, servletMap: Map[String, Class[_]]): WebServer = {
    val ws = new WebServer(port, webContentURL)
    for ((path, clazz) <- servletMap) {
      ws.registerServlet(clazz.getName, path)
    }
    try {
      ws.start()
    } catch {
      case _: Throwable =>
    }
    ws
  }
}