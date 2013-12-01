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

import vcc.util.swing.MigPanel
import scala.swing.TextField

object DamageEffectEditor {

  trait View {
    def setConditionText(condition: String)

    def setDamageText(damage: String)

  }

  case class Memento(damage: String, condition: String) {
    def asListText = damage + "; " + condition
  }
}

class DamageEffectEditor(presenter: DamageEffectEditorPresenter) extends MigPanel("fillx", "[fill,grow]", "") with DamageEffectEditor.View {

  private val damageField = new TextField()
  private val conditionField = new TextField()

  init()

  def setDamageText(damage: String) {
    damageField.text = damage
  }

  def setConditionText(condition: String) {
    conditionField.text = condition
  }

  private def init() {
    presenter.bind(this)

    damageField.name = "dee.damage"
    add(damageField, "wrap")

    conditionField.name = "dee.condition"
    add(conditionField, "wrap")
  }
}