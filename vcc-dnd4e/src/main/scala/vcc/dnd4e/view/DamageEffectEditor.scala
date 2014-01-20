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

import vcc.util.swing.MigPanel
import scala.swing.{Label, TextField}
import vcc.dnd4e.view.DamageEffectEditor.Memento
import scala.swing.event.ValueChanged
import vcc.dnd4e.view.GroupFormPanel.FormValueChanged

object DamageEffectEditor {

  case class Memento(damage: String, condition: String) {
    def asListText = damage + "; " + condition
  }
}

class DamageEffectEditor extends MigPanel("fillx", "[fill,grow]", "")
  with GroupFormPanel.Presenter[Memento] {

  private val diceRE = """^\d+d\d+\+\d+$""".r
  private val damageField = new TextField()
  private val conditionField = new TextField()

  init()

  listenTo(damageField)
  reactions += {
    case ValueChanged(this.damageField) =>
      publish(FormValueChanged(this, isValid))
  }

  private def init() {
    damageField.name = "dee.damage"
    add(new Label("Damage:"), "wrap")
    add(damageField, "wrap")

    conditionField.name = "dee.condition"
    add(new Label("Condition:"), "wrap")
    add(conditionField, "wrap")
  }

  def setEntry(entry: Memento) {
    damageField.text = entry.damage
    conditionField.text = entry.condition
  }

  def getEntry: Memento = {
    Memento(
      damageField.text,
      conditionField.text
    )
  }

  def clear(): Unit = {
    damageField.text = ""
    conditionField.text = ""
  }

  def isValid: Boolean = {
    diceRE.pattern.matcher(damageField.text).matches()
  }
}