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
import scala.swing.event.{ListSelectionChanged, ButtonClicked, SelectionChanged}
import scala.swing.ListView.IntervalMode

object GroupFormPanel {

  trait Form[T] {
    def setEntry(entry: T)
  }

}

class GroupFormPanel[T](formComponent: Panel with GroupFormPanel.Form[T]) extends CardPanel {
  private val groupList = new ListView[T]()
  groupList.name = "group.list"
  groupList.selection.intervalMode = IntervalMode.Single

  private val backButton = new Button(Action("Back") {
    groupList.selection.indices.clear()
    showCard("group")
  })
  backButton.name = "form.back"

  def setContent(newContent: Seq[T]) {
    groupList.listData = newContent
    showCard("group")
  }

  private val fromPanel = new MigPanel("") {
    add(backButton, "wrap")
    add(formComponent, "wrap")
  }

  addCard(groupList, "group")
  addCard(fromPanel, "form")

  listenTo(groupList.selection)

  showCard("form")

  reactions += {
    case ListSelectionChanged(this.groupList, range, false) =>
      showCard("form")
      formComponent.setEntry(groupList.listData(groupList.selection.leadIndex))
  }
}