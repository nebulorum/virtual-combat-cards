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

import scala.swing.ListView
import scala.swing.event.ListSelectionChanged
import vcc.util.swing.MigPanel
import scala.swing.ListView.IntervalMode

object DamageEffectPanel {

  trait View {
    def setListContent(content: Seq[Entry])
  }

  class Entry(val name: String, val desc:String) {
    def asListText = s"""<html><body><strong>$name</strong><br/>$desc</body></html>"""
  }
}

class DamageEffectPanel(presenter: DamageEffectPresenter) extends MigPanel("") with DamageEffectPanel.View {
   private val list = new ListView[String]()

  init()

  def setListContent(content: Seq[DamageEffectPanel.Entry]) {
    list.listData = content.map(_.asListText)
  }

  private def init() {
    list.name = "list.memento"
    list.selection.intervalMode = IntervalMode.Single
    add(list, "")

    listenTo(list.selection)

    reactions += {
      case ListSelectionChanged(_, range, true) =>
        presenter.switchSelection(list.selection.leadIndex)
    }
  }
}