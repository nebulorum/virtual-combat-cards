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
import scala.swing.Frame
import vcc.dnd4e.view.DamageEffectPanel.Entry
import org.mockito.Mockito._
import vcc.dnd4e.view.DamageEffectEditor.Memento
import org.mockito.Matchers

class DamageEffectPanelTest extends UISpecTestCase {

  val sampleContent: Seq[Entry] = Seq(
    new DamageEffectPanel.Entry("One", Memento("Some Damage", "Some desc")),
    new DamageEffectPanel.Entry("Two", Memento("Damage", "Another desc")))
  private val newEntry = new Entry("New", Memento("1d6+4", "Other"))

  private var view: DamageEffectPanel.View = null
  private val presenter: DamageEffectPresenter = mock(classOf[DamageEffectPresenter])
  private val editorPresenter: DamageEffectEditorPresenter = mock(classOf[DamageEffectEditorPresenter])

  override def setUp() {
    super.setUp()
    val panel = new DamageEffectPanel(presenter, editorPresenter)
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
    verify(presenter).bind(view)
    verify(editorPresenter).bind(Matchers.anyObject[DamageEffectEditor.View]())
  }

  def testAddElementsToMemento() {
    view.setListContent(sampleContent)
    assertTrue(getList.contentEquals(sampleContent.map(_.asListText): _*))
  }

  def testSelectFromList_shouldUpdateNameField() {
    view.setListContent(sampleContent)
    getList.click(0)
    verify(presenter).switchSelection(sampleContent(0).id)
  }

  def testSelectFromList_shouldSelectOnlyOne() {
    view.setListContent(sampleContent)
    getList.click(0)
    getList.click(1)
    verify(presenter).switchSelection(sampleContent(1).id)
  }

  def testSelection_shouldMaintainCurrentSelectionIfStillInList() {
    view.setListContent(sampleContent)
    getList.click(1)
    view.setListContent(Seq(newEntry) ++ sampleContent)
    assertThat(getList.selectionEquals(sampleContent(1).asListText))
  }

  def testSelection_shouldClearSelectionIfCurrentNotInList() {
    view.setListContent(sampleContent)
    getList.click(1)
    view.setListContent(Seq(newEntry))
    assertThat(getList.selectionIsEmpty())
  }

  def testRemoveButton_shouldBeThereButDisableWhenListIsEmpty() {
    assertFalse(getRemove.isEnabled)
    assertThat(getRemove.tooltipEquals("Remove effect and damage"))
  }

  def testRemoveButton_shouldBeEnabledOnSelectionAndCallPresenter() {
    view.setListContent(sampleContent)
    getList.click(1)
    getRemove.click()
    verify(presenter).removeEntry(sampleContent(1).id)
  }

  def testUpdateName() {
    view.setName("The name")
    assertThat(getMainWindow.getTextBox("dep.name").textEquals("The name"))
  }

  private def getRemove = {
    getMainWindow.getButton("button.remove")
  }

  private def getList = {
    getMainWindow.getListBox("list.memento")
  }
}