/*
 *  Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.view

import scala.swing._
import vcc.dnd4e.view.dialog.FileChooserHelper
import vcc.dnd4e.view.compendium.CompendiumMenu
import vcc.dnd4e.view.helper.PartyLoader
import java.net.URL
import javax.swing.KeyStroke
import java.awt.Desktop
import vcc.util.swing.SwingHelper
import vcc.dnd4e.application.Application
import vcc.updater.UpdateManager

/**
 * Helper object to create MenuItem associated to PanelDirector properties
 */
object PropertyMenuItem {

  /**
   * Create a CheckMenuItem associated with a boolean PanelDirector property.
   */
  def createCheckMenu(name: String, director: PanelDirector, prop: PanelDirector.property.Value): MenuItem = {
    val mi = new CheckMenuItem(name)
    mi.selected = director.getBooleanProperty(prop)
    mi.action = Action(name) {
      director.setProperty(prop, mi.selected)
    }
    mi
  }
}

trait ConfigurationPanelCallback {
  def showConfiguration()
}

class MainMenu(director: PanelDirector, docker: CustomDockingAdapter, parent: Frame,
               configurationPanel: ConfigurationPanelCallback, releaseInformation: ReleaseInformation)
  extends MenuBar {

  private val logger = org.slf4j.LoggerFactory.getLogger("user")
  private val webServerBase = "http://127.0.0.1:4143"

  private val fileMenu = new Menu("File")
  fileMenu.contents += new MenuItem(Action("Load Party ...") {
    val file = FileChooserHelper.chooseOpenFile(this.peer, FileChooserHelper.partyFilter)
    if (file.isDefined) {
      PartyLoader.getInstance(director, Component.wrap(parent.peer.getRootPane)).loadToBattle(file.get)
    }
  })
  fileMenu.contents += new Separator()
  fileMenu.contents += new MenuItem(Action("Save combat ...") {
    val file = FileChooserHelper.chooseSaveFile(this.peer, FileChooserHelper.combatSaveFilter)
    if (file.isDefined) {
      tryExecutionReportingError("Failed to save combat") {
        Application.getInstance.saveStateToFile(file.get)
      }
    }
  })
  fileMenu.contents += new MenuItem(Action("Load combat ...") {
    val file = FileChooserHelper.chooseOpenFile(this.peer, FileChooserHelper.combatSaveFilter)
    if (file.isDefined) {
      tryExecutionReportingError("Failed to load combat") {
        Application.getInstance.loadStateFile(file.get)
      }
    }
  })
  fileMenu.contents += new Separator()
  fileMenu.contents += new MenuItem(Action("Preferences ...") {
    configurationPanel.showConfiguration()
  })

  private val combatMenu = new CombatMenu(director, parent)

  private val historyMenu = new Menu("History")
  historyMenu.contents += new MenuItem(new Action("Undo") {
    def apply() {
      director.requestUndo()
    }

    accelerator = Some(KeyStroke.getKeyStroke('Z'.toInt, java.awt.Event.CTRL_MASK))
  })
  historyMenu.contents += new MenuItem(new Action("Redo") {
    def apply() {
      director.requestRedo()
    }

    accelerator = Some(KeyStroke.getKeyStroke('Y'.toInt, java.awt.Event.CTRL_MASK))
  })
  historyMenu.contents += new Separator
  historyMenu.contents += new MenuItem(Action("Clear History") {
    director.requestClearHistory()
  })

  private val viewMenu = new Menu("View")
  private val hideDeadMenu = PropertyMenuItem.createCheckMenu("Hide Dead", director, PanelDirector.property.HideDead)
  private val robinViewMenu = PropertyMenuItem.createCheckMenu("Show Next Up as first combatant", director, PanelDirector.property.RobinView)

  viewMenu.contents += hideDeadMenu
  viewMenu.contents += robinViewMenu
  viewMenu.contents += new Separator
  viewMenu.contents += makeWebBrowserMenu("Player View", webServerBase)

  private val dockMenu = new Menu("Dockable")

  private val dockRestoreMenu = new Menu("Restore")
  private val dockFocusMenu = new Menu("Go to window")
  dockMenu.contents += dockRestoreMenu
  dockMenu.contents += dockFocusMenu
  dockMenu.contents += new Separator
  dockMenu.contents += new MenuItem(Action("Restore Default Layout") {
    docker.restoreDefaultLayout()
  })
  dockMenu.contents += new MenuItem(Action("Save Layout") {
    docker.storeLayoutToFile(Component.wrap(parent.peer.getRootPane))
  })
  dockMenu.contents += new MenuItem(Action("Load Layout") {
    docker.restoreLayoutFromFile(Component.wrap(parent.peer.getRootPane))
  })

  def addToDockRestoreMenu(item: MenuItem) {
    dockRestoreMenu.contents += item
  }

  def addToDockRestoreFocusMenu(item: MenuItem) {
    dockFocusMenu.contents += item
  }

  private val helpMenu = new Menu("Help")
  helpMenu.contents += makeWebBrowserMenu("Online Manual", "http://www.exnebula.org/vcc/manual")

  helpMenu.contents += makeWebBrowserMenu("Manual", webServerBase + "/manual")

  helpMenu.contents += makeWebBrowserMenu("Firefox plugin", "http://www.exnebula.org/vcc/plugin")
  helpMenu.contents += makeWebBrowserMenu("Chrome plugin", "https://chrome.google.com/webstore/detail/virtual-combat-cards-capt/acaljigfeambgpaebnjigclobfepkdhe")

  private def makeWebBrowserMenu(menuText: String, link: String): MenuItem = {
    new MenuItem(Action(menuText) {
      val dsk = Desktop.getDesktop
      dsk.browse(new URL(link).toURI)
    })
  }

  helpMenu.contents += new MenuItem(Action("Check for Updates ...") {
    SwingHelper.invokeInOtherThread {
      logger.info("Update manager: Starting update")
      logger.info("Update manager: Fetch version from URL: " + releaseInformation.versionReleaseURL)
      UpdateManager.runUpgradeProcess(releaseInformation.versionReleaseURL, releaseInformation.currentVersion, IconLibrary.MetalD20.getImage, 0)
      logger.info("Update manager: End update")
    }
  })

  helpMenu.contents += new Separator
  helpMenu.contents += new MenuItem(Action("About") {
    showAboutDialog()
  })

  contents += fileMenu
  contents += combatMenu
  contents += historyMenu
  contents += viewMenu
  contents += new CompendiumMenu(director)
  contents += dockMenu
  contents += helpMenu

  private def tryExecutionReportingError(errorMessage: String)(executionBlock: => Unit) {
    try {
      executionBlock
    } catch {
      case exception: Throwable =>
        logger.warn(errorMessage, exception)
        Dialog.showMessage(null, errorMessage + ". Report was added to log.", errorMessage, messageType = Dialog.Message.Error)
    }
  }

  def showAboutDialog() {
    Dialog.showMessage(
      Component.wrap(parent.peer.getRootPane),
      "This is Virtual Combant Cards version: " + releaseInformation.currentVersion.versionString +
        "\nDesigned at: www.exnebula.org",
      "About Virtual Combat Cards",
      Dialog.Message.Info, IconLibrary.MetalD20)
  }
}