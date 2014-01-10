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
import vcc.dnd4e.view.GroupFormPanel.FormValueChanged
import javax.swing.BorderFactory

object GroupFormPanel {

  trait Form[T] extends Publisher {
    def setEntry(entry: T)
  }

  case class FormValueChanged(form: Component, valid: Boolean) extends Event

}

class GroupFormPanel[T](formComponent: Panel with GroupFormPanel.Form[T]) extends CardPanel {
  private val groupList = new ListView[T]()
  groupList.name = "group.list"
  groupList.selection.intervalMode = IntervalMode.Single
  groupList.border = BorderFactory.createBevelBorder(1)

  private val backButton = new Button(Action("Back") {
    groupList.selection.indices.clear()
    showCard("group")
  })
  backButton.name = "form.back"
  backButton.icon = IconLibrary.ThreeBarIcon

  private val saveButton = new Button(Action("") {

  })
  saveButton.name = "form.save"
  saveButton.enabled = false
  saveButton.icon = IconLibrary.DiskIcon

  def setContent(newContent: Seq[T]) {
    groupList.listData = newContent
    showCard("group")
  }

  private val formPanel = new MigPanel("fill, ins 0, debug", "[][][]", "[grow 0][fill][grow 0]") {
    add(backButton, "wrap")
    add(formComponent, "span 3, growx, wrap")
    add(saveButton, "")
  }

  addCard(groupList, "group")
  addCard(formPanel, "form")

  listenTo(groupList.selection, formComponent)

  showCard("form")

  reactions += {
    case ListSelectionChanged(this.groupList, range, false) =>
      showCard("form")
      formComponent.setEntry(groupList.listData(groupList.selection.leadIndex))
    case FormValueChanged(this.formComponent, valid) =>
      saveButton.enabled = valid
  }
}