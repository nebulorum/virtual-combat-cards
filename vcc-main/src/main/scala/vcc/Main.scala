/*
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc

import vcc.infra.startup._
import vcc.infra.LogService
import vcc.util.swing.SwingHelper
import javax.swing.JOptionPane
import org.slf4j.LoggerFactory

object Main {

  def warnFailure(win: java.awt.Window) {
    JOptionPane.showMessageDialog(win,"Virtual Combat Cards initialization failed. Please check launch.log for clues.\n"+
                                  "Check http://www.exnebula.org/vcc to look for help or report an issue.",
                                  "Initialization failed",JOptionPane.ERROR_MESSAGE)
  }
  
  def main(args : Array[String]) {

    LogService.initializeStartupLog()
    
    val logger = LoggerFactory.getLogger("startup")

    val frame = SplashWindow.showSplash(vcc.dnd4e.BootStrap)
    if(frame != null) {
      SwingHelper.invokeLater{frame.visible = true}
      logger.info("Initialization complete.")
    } else {
      warnFailure(SplashWindow)
      logger.error("Initialization failed.")
      sys.exit(1)
    }
  }
}