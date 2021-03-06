/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.compendium

import vcc.util.swing.{MigPanel, XHTMLPaneAgent, XHTMLPane}
import swing.{Button, Action, MainFrame}
import vcc.infra.xtemplate.TemplateDataSource
import java.io.{FileInputStream, File}
import vcc.dndi.reader.{DNDInsiderCapture, DNDIObject}
import javax.swing.{JComponent, JFileChooser}
import javax.swing.filechooser.FileFilter

object StatBlockGenerateAndView {
  private val baseDir: File = {
    val possibleDirs = Seq(new File("vcc-web/src/main/webapp"), new File("../vcc-web/src/main/webapp"))
    possibleDirs.find(dir => dir.isDirectory).get
  }

  CaptureTemplateEngine.initialize(baseDir)
  XHTMLPaneAgent.createInstance(baseDir)
  println("File exists: " + (baseDir.getAbsolutePath) + (if (baseDir.exists) " exists" else " not exists"))

  var monster: DNDIObject = null

  def loadMonster(file: File): DNDIObject = {
    monster = try {
      DNDInsiderCapture.captureEntry(new FileInputStream(file), DNDInsiderCapture.NullEntityStore) match {
        case None => null
        case p@Some(DNDInsiderCapture.UnsupportedEntity(_, _)) =>
          println("Failed to load entity: " + p)
          null
        case Some(DNDInsiderCapture.CapturedEntity(obj)) => obj
      }
    } catch {
      case e: Exception =>
        System.err.println("Failed to parse: " + file + " reason: " + e.getMessage)
        null
    }
    monster
  }

  def regenerateTemplate() {
    if (monster != null) {
      try {
        val template = CaptureTemplateEngine.getInstance.fetchClassTemplate(monster.clazz)
        val xml = template.render(monster.asInstanceOf[TemplateDataSource])
        xhtmlPane.setDocumentFromText(xml.toString())
      } catch {
        case e: Exception =>
          e.printStackTrace()
          xhtmlPane.setDocumentFromText("<html><body>" + e.getMessage + "</body></html>")
      }
    }
  }

  val xhtmlPane = new XHTMLPane()
  val reloadButton = new Button(Action("Regenerate template") {
    this.regenerateTemplate()
  })

  lazy val loadMonsterButton = new Button(Action("Load monster") {
    val fileOption: Option[File] = FileChooserHelper.chooseOpenFile(this.mainWindow.peer.getRootPane, null)
    if (fileOption.isDefined && this.loadMonster(fileOption.get) != null) {
      this.regenerateTemplate()
    }
  })


  val mainWindow: MainFrame = new MainFrame {
    title = "StatBlock Generator Test"
    contents = new MigPanel("fill", "[250]", "[40][500]") {
      add(loadMonsterButton, "split 2")
      add(reloadButton, "wrap")
      add(xhtmlPane, "growx, growy")
    }
  }

  def main(args: Array[String]) {
    org.apache.log4j.BasicConfigurator.configure()
    if (args.length > 0) {
      val file = new File(args(0))
      if (file.exists && file.isDirectory) {
        FileChooserHelper.setLastDirectory(file.getParentFile)
      }
    }

    mainWindow.visible = true
  }

  private object FileChooserHelper {
    private var lastDirectory = new File(System.getProperty("user.dir"))

    def setLastDirectory(newDirectory: File) {
      synchronized {
        lastDirectory = newDirectory
      }
    }

    def chooseOpenFile(over: JComponent, filter: FileFilter): Option[File] = {
      val fileDialog = new JFileChooser(getWorkDirectory)
      if (filter != null) fileDialog.setFileFilter(filter)

      val result = fileDialog.showOpenDialog(over)
      if (result == JFileChooser.APPROVE_OPTION) {
        setLastDirectory(fileDialog.getSelectedFile.getParentFile)
        Some(fileDialog.getSelectedFile)
      } else
        None
    }

    private def getWorkDirectory: File = lastDirectory

  }
}