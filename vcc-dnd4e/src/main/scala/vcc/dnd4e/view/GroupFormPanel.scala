/*
 * Copyright (C) 2013-2014 - Thomas Santana <tms@exnebula.org>
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

    def clear(): Unit

    def isValid: Boolean
  }

  case class FormValueChanged(form: Component, valid: Boolean) extends Event

  case class FormSave(form: Component) extends Event

}

class GroupFormPanel[T](formComponent: Panel with GroupFormPanel.Form[T]) extends CardPanel {
  private val groupList = createGroupList()
  private val backButton = createBackButton()
  private val saveButton = createSaveButton()
  private val newButton = createNewButton()
  private val deleteButton = createDeleteButton()
  private val copyButton = createCopyButton()
  private val formPanel = createFormPanel()
  private val groupPanel = createGroupPanel()
  private var currentSelectedEntry: Option[Int] = None

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
      add(deleteButton, "")
      add(copyButton, "")
    }
    panel
  }

  private def createGroupPanel() = {
    val panel = new MigPanel("") {
      add(newButton, "wrap")
      add(groupList)
    }
    panel
  }

  private def init() {
    addCard(groupPanel, "group")
    addCard(formPanel, "form")
    showFormCard()

    listenTo(groupList.selection, formComponent)

    reactions += {
      case ListSelectionChanged(this.groupList, range, false) if !currentSelectedEntry.isDefined =>
        currentSelectedEntry = groupList.selection.indices.headOption
        if (currentSelectedEntry.isDefined)
          formComponent.setEntry(groupList.listData(currentSelectedEntry.get))
        showFormCard()
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

  private def createNewButton() = {
    val button = new Button(Action("") {
      formComponent.clear()
      showFormCard()
    })
    button.name = "group.newButton"
    button.icon = IconLibrary.AddIcon
    button
  }

  private def createDeleteButton() = {
    val button = new Button(Action("") {
      groupList.listData = deleteEntry(currentSelectedEntry.get, groupList.listData)
      showGroupCard()
    })
    button.name = "form.delete"
    button.icon = IconLibrary.DeleteIcon
    button
  }

  private def createCopyButton() = {
    val button = new Button(Action("") {
      groupList.listData = duplicateEntry(currentSelectedEntry.get, groupList.listData)
    })
    button.name = "form.copy"
    button.icon = IconLibrary.PageCopyIcon
    button.enabled = false
    button
  }

  private def duplicateEntry(pos: Int, data: Seq[T]) = data.take(pos+1) ++ data.drop(pos)

  private def deleteEntry(pos: Int, data: Seq[T]) = data.take(pos) ++ data.drop(pos+1)

  private def doSave() {
    val oldList = groupList.listData
    val newList: Seq[T] = if (currentSelectedEntry.isDefined)
      oldList.updated(groupList.selection.leadIndex, formComponent.getEntry)
    else
      formComponent.getEntry +: oldList
    groupList.listData = newList
  }

  private def showGroupCard() {
    groupList.peer.clearSelection()
    groupList.selection.indices.clear()
    currentSelectedEntry = None
    if (groupList.listData.isEmpty) {
      formComponent.clear()
      showFormCard()
    } else
      showCard("group")
  }

  private def showFormCard() {
    val isOldEntry = currentSelectedEntry.isDefined
    deleteButton.enabled = isOldEntry
    saveButton.enabled = formComponent.isValid
    backButton.enabled = !groupList.listData.isEmpty
    copyButton.enabled = currentSelectedEntry.isDefined
    showCard("form")
  }
}