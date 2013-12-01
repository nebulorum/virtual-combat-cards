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

import scala.swing.MainFrame
import java.awt.Dimension
import vcc.dnd4e.view.DamageEffectPanel.Entry
import vcc.dnd4e.view.DamageEffectEditor.Memento

object DamageEffectView {

  private val editorPresenter = new DamageEffectEditorPresenter
  private val presenter = new DamageEffectPresenter(editorPresenter)
  private val panel: DamageEffectPanel = new DamageEffectPanel(presenter, editorPresenter)

  def main(args: Array[String]) {
    val f = new MainFrame {
      title = "Damage Effect Test"
      contents = panel
      minimumSize = new Dimension(300, 500)
    }
    f.pack()
    f.visible = true
    scala.swing.Swing.onEDT {
      presenter.setContent(new DamageEffectPanelTest().sampleContent ++ Seq(new Entry("Other", Memento("Other", "Condition"))))
    }
  }
}