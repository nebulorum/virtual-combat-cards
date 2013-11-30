/*
 * Copyright (C) 2013-2013 - Thomas Santana <tms@exnebula.org>
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

import org.uispec4j.{Window, UISpecAdapter, UISpecTestCase}
import scala.swing.{MainFrame, Frame}
import java.awt.Dimension
import vcc.dnd4e.view.DamageEffectPanel.Entry
import org.mockito.Mockito._

object DamageEffectView {

  private val presenter = new DamageEffectPresenter
  private val panel: DamageEffectPanel = new DamageEffectPanel(presenter)

  def main(args: Array[String]) {
    val f = new MainFrame {
      title = "Damage Effect Test"
      contents = panel
      minimumSize = new Dimension(300,500)
    }
    panel.setListContent(new DamageEffectPanelTest().sampleContent)
    f.pack()
    f.visible = true
  }
}

class DamageEffectPanelTest extends UISpecTestCase {

  val sampleContent: Seq[Entry] = Seq(
    new DamageEffectPanel.Entry("One", "Some desc"),
    new DamageEffectPanel.Entry("Two", "Another desc"))
  
  private var view:DamageEffectPanel.View = null
  private val presenter:DamageEffectPresenter = mock(classOf[DamageEffectPresenter])

  override def setUp() {
    super.setUp()
    val panel = new DamageEffectPanel(presenter)
    view = panel
    setAdapter(new UISpecAdapter() {
      def getMainWindow: Window = {
        val frame = new Frame {
          contents = panel
        }
        new Window(frame.peer)
      }
    })
  }

  def testOpenFrame() {
    assertTrue(getList.isEnabled)
  }

  def testAddElementsToMemento() {
    view.setListContent(sampleContent)
    assertTrue(getList.contentEquals(sampleContent.map(_.asListText) : _*))
  }

  def testSelectFromList_shouldUpdateNameField() {
    view.setListContent(sampleContent)
    getList.click(0)
    verify(presenter).switchSelection(0)
  }

  def testSelectFromList_shouldSelectOnlyOne() {
    view.setListContent(sampleContent)
    getList.click(0)
    getList.click(1)
    verify(presenter).switchSelection(1)
  }

  private def getList  = {
    getMainWindow.getListBox("list.memento")
  }
}