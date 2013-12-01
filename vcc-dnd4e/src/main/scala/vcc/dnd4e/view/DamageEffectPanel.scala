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

import scala.swing.{ScrollPane, Button, ListView}
import scala.swing.event.{ButtonClicked, ListSelectionChanged}
import vcc.util.swing.MigPanel
import scala.swing.ListView.IntervalMode
import javax.swing.DefaultListCellRenderer

object DamageEffectPanel {

  object EntryIdGenerator {
    private var nextEntryId = 0
    def nextId(): Int = {
      nextEntryId = nextEntryId + 1
      nextEntryId
    }
  }

  trait View {
    def setListContent(content: Seq[Entry])
  }

  class Entry(val name: String, val desc:String) {
    val id:Int = EntryIdGenerator.nextId()

    def asListText = s"""<html><body><strong>$name</strong><br/>&nbsp;$desc</body></html>"""

    override def toString: String = s"Entry($id, $name, $desc)"
  }
}

class DamageEffectPanel(presenter: DamageEffectPresenter) extends MigPanel("flowy, fillx, filly") with DamageEffectPanel.View {
  import DamageEffectPanel._
  private val list = new ListView[Entry]() {
    renderer = ListView.Renderer[Entry,String](_.asListText)(ListView.Renderer.wrap(new DefaultListCellRenderer()))
  }
  private val removeButton = new Button("-")

  private var currentSelection: Option[Int] = None

  init()

  def setListContent(content: Seq[Entry]) {
    list.listData = content
    adjustSelection()
  }


  private def adjustSelection() {
    if (currentSelection.isDefined) {
      val idx = list.listData.indexWhere(_.id == currentSelection.get)
      if (idx >= 0) {
        list.selectIndices(idx)
      } else {
        list.selectIndices()
        currentSelection = None
      }
    }
  }

  private def init() {
    presenter.bind(this)

    list.name = "list.memento"
    list.selection.intervalMode = IntervalMode.Single
    add(new ScrollPane(list), "w 300")

    removeButton.name = "button.remove"
    removeButton.enabled = false
    removeButton.tooltip = "Remove effect and damage"
    add(removeButton, "")

    listenTo(list.selection, removeButton)

    reactions += {
      case ListSelectionChanged(_, range, true) =>
        currentSelection = Some(list.listData(list.selection.leadIndex).id)
        presenter.switchSelection(currentSelection.get)
        removeButton.enabled = currentSelection.isDefined
      case ButtonClicked(this.removeButton) =>
        presenter.removeEntry(currentSelection.get)

    }
  }
}