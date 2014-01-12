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

import vcc.util.swing.{MigPanel, CardPanel}
import scala.swing._
import scala.swing.event.{Event, ListSelectionChanged}
import scala.swing.ListView.IntervalMode
import vcc.dnd4e.view.GroupFormPanel.{FormSave, FormValueChanged}
import javax.swing.BorderFactory

object GroupFormPanel {

  trait Form[T] extends Publisher {
    def setEntry(entry: T)

    def getEntry: T

    def isValid: Boolean
  }

  case class FormValueChanged(form: Component, valid: Boolean) extends Event

  case class FormSave(form: Component) extends Event

}

class GroupFormPanel[T](formComponent: Panel with GroupFormPanel.Form[T]) extends CardPanel {
  private val groupList = createGroupList()
  private val backButton = createBackButton()
  private val saveButton = createSaveButton()
  private val formPanel = createFormPanel()

  init()

  def setContent(newContent: Seq[T]) {
    groupList.listData = newContent
    showGroupCard()
  }

  def getContent: Seq[T] = groupList.listData

  private def createFormPanel() = {
    val panel = new MigPanel("fill, ins 0", "[][][]", "[grow 0][fill][grow 0]") {
      add(backButton, "wrap")
      add(formComponent, "span 3, growx, wrap")
      add(saveButton, "")
    }
    panel
  }

  private def init() {
    addCard(groupList, "group")
    addCard(formPanel, "form")
    showFormCard()

    listenTo(groupList.selection, formComponent)

    reactions += {
      case ListSelectionChanged(this.groupList, range, false) =>
        showFormCard()
        formComponent.setEntry(groupList.listData(groupList.selection.leadIndex))
      case FormValueChanged(this.formComponent, valid) =>
        saveButton.enabled = valid
      case FormSave(this.formComponent) =>
        if (formComponent.isValid) doSave()
    }
  }

  private def createGroupList() = {
    val list = new ListView[T]()
    list.name = "group.list"
    list.selection.intervalMode = IntervalMode.Single
    list.border = BorderFactory.createBevelBorder(1)
    list
  }

  def createBackButton() = {
    val button = new Button(Action("") {
      groupList.selection.indices.clear()
      showGroupCard()
    })
    button.name = "form.back"
    button.icon = IconLibrary.ThreeBarIcon
    button
  }

  private def createSaveButton() = {
    val button = new Button(Action("") {
      doSave()
      showGroupCard()
    })
    button.name = "form.save"
    button.enabled = false
    button.icon = IconLibrary.DiskIcon
    button
  }

  private def doSave() {
    groupList.listData = groupList.listData.updated(groupList.selection.leadIndex, formComponent.getEntry)
  }

  private def showGroupCard() {
    showCard("group")
  }
  private def showFormCard() {
    showCard("form")
  }
}