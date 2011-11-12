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
package vcc.dnd4e

import vcc.util.{UpdateManager, PackageUtil}
import vcc.model.Registry
import vcc.dnd4e.compendium.Compendium
import vcc.infra.startup._
import vcc.infra.ConfigurationFinder
import vcc.infra.LogService
import vcc.infra.datastore.DataStoreFactory
import vcc.util.swing.XHTMLPaneAgent
import java.io.File
import vcc.dndi.servlet.{CaptureServlet, CaptureHoldingArea}

object BootStrap extends StartupRoutine {
  val logger = org.slf4j.LoggerFactory.getLogger("startup")

  /**
   * Return current version that is being used, can be faked by setting System property. vcc.fake.version
   */
  val version = {
    val fakeVersion = UpdateManager.Version.fromString(System.getProperty("vcc.fake.version"))
    if (fakeVersion != null) fakeVersion
    else UpdateManager.Version.fromVersionFileFromStream(this.getClass.getResourceAsStream("/vcc/version.xml"))
  }

  def start(srw: StartupReportWindow): scala.swing.Frame = {
    var createCompendium = false
    var importSampleCompendium = false
    logger.info("Starting VCC DND4E components...")
    logger.debug("VCC version {} starting", version)

    callStartupStep(srw, "Loading configuration") {
      if (!ConfigurationFinder.foundConfiguration) {
        logger.info("Can't find the configuration, will configure")
        val cdiag = new ConfigurationDialog(srw.ownerWindow, true)
        val result = cdiag.promptUser()
        if (result == None) {
          logger.warn("Failed to complete configuration, will exit")
          return null
        } else {
          createCompendium = true
          importSampleCompendium = result.get
          logger.debug("Create compendium requested, import sample? " + importSampleCompendium)
        }
      }
      Configuration.load(ConfigurationFinder.locateFile())
      Configuration
    }
    callStartupStep(srw, "Logging") {
      val logs = Seq("org.mortbay.log", "domain", "app", "infra", "user", "fs-agent")
      val file = new File(Configuration.baseDirectory.value, "vcc.log")
      logger.info("Starting to logging operations to: {}", file)
      LogService.initializeLog(logs, file.toString, LogService.level.Debug, Configuration.storeLogs.value)
      LogService
    }
    // Compendium depends on active Compendium settings
    callStartupStep(srw, "Compendium components") {
      Compendium
    }

    if (createCompendium) {
      callStartupSimpleBlock(srw, "Create user compendium") {
        val dsid = Configuration.compendiumStoreID.value
        val esb = DataStoreFactory.getDataStoreBuilder(dsid)
        if (esb.exists(dsid)) {
          logger.warn("Compendium exists, will assume it is valid and attempt to load")
          true
        } else {
          val es = esb.create(dsid)
          if (es == null) {
            logger.error("Failed to create compendium {}", Configuration.compendiumStoreID.value)
            false
          } else {
            val sampleFile = new File(UpdateManager.getInstallDirectory, "sample-comp.zip")
            if (importSampleCompendium) {
              logger.info("Importing sample compendium from {} if it exists.", sampleFile)
              if (sampleFile.exists) {
                logger.debug("Extracting sample data")
                PackageUtil.extractFilesFromZip(sampleFile, Configuration.baseDirectory.value)
              }
            }
            true
          }
        }
      }
    }
    callStartupSimpleBlock(srw, "Load Compendium") {
      import vcc.infra.datastore.naming.DataStoreURI
      import vcc.dnd4e.compendium.CompendiumRepository
      import javax.swing.JOptionPane

      val compendiumID = Configuration.compendiumStoreID.value
      logger.info("Opening compendium: {}", compendiumID)

      val compendium = try {
        new CompendiumRepository(compendiumID)
      } catch {
        case e =>
          logger.error("Failed compendium load", e)
          val ret = JOptionPane.showConfirmDialog(srw.ownerWindow,
            "Failed to open Compendium. This may be due to missing files or directory. You may have accidentally\n" +
              "removed the compendium directory. Virtual Combat Cards can create an empty Compendium folder.\n" +
              "Selecting yes will recreate a blank compendium, or no to cancel this run (you will see another error message)." +
              "\n\nCreate new empty compendium directory?",
            "Failed to open Compendium",
            JOptionPane.YES_NO_OPTION)
          if (ret == 0) {
            logger.info("Creating new repository at : {}", compendiumID)
            val esb = DataStoreFactory.getDataStoreBuilder(compendiumID)
            esb.create(compendiumID)
            new CompendiumRepository(compendiumID)
          } else
            null
      }
      if (compendium == null) {
        logger.warn("Failed to load compendium {}, will exit", compendiumID)
      } else {
        logger.info("Opened compendium {}", compendiumID)
        Registry.register("Compendium", compendiumID)
        Registry.register(compendiumID, compendium)
        Compendium.setActiveRepository(compendium)
      }
      Registry.get[DataStoreURI]("Compendium").isDefined && Registry.get[CompendiumRepository](Registry.get[DataStoreURI]("Compendium").get).isDefined
    }

    callStartupStep(srw, "Web Server") {
      import vcc.infra.webserver.WebServer
      CaptureHoldingArea.initialize(new File(Configuration.baseDirectory.value, "dndicache"))
      WebServer.initialize("webserver", 4143, Map(
        "/capture" -> classOf[CaptureServlet]
      ))
      Registry.get[WebServer]("webserver").get
    }

    callStartupStep(srw, "Core Tracker") {
      import vcc.controller.Tracker
      import vcc.dnd4e.domain.tracker.transactional.{CombatController, CombatContext}
      import vcc.dnd4e.domain.tracker.common.CombatStateRules

      Tracker.initialize(new CombatController(new CombatStateRules(), new CombatContext()))

      //Make sure it got registered
      Registry.get[scala.actors.Actor]("tracker").get.asInstanceOf[StartupStep]
    }

    callStartupStep(srw, "User Interface Elements") {
      vcc.dnd4e.view.compendium.DNDICaptureMonitor
      XHTMLPaneAgent.createInstance(Configuration.dataDirectory)
      XHTMLPaneAgent
    }

    srw.reportProgress(this, "Initialization complete.")
    new vcc.dnd4e.view.MasterFrame(version)
  }
}