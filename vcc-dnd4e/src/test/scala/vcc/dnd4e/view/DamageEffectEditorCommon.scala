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

import org.uispec4j.{Window, UISpecAdapter, UISpecTestCase}
import org.mockito.Mockito._
import scala.Some
import vcc.dnd4e.tracker.common.{UnifiedCombatantID, InitiativeOrderID, CombatantID}
import scala.swing.Frame

trait DamageEffectEditorCommon extends UISpecTestCase {
  protected var view: DamageEffectEditor = null
  protected val tracker = mock(classOf[PanelDirector])

  protected val combA = CombatantID("A")
  protected val combB = CombatantID("B")
  protected val ioiA = InitiativeOrderID(combA, 0)
  protected val ioiB = InitiativeOrderID(combB, 0)
  protected val ucAO = UnifiedCombatantID(ioiA)
  protected val ucBO = UnifiedCombatantID(ioiB)
  protected val ucB = UnifiedCombatantID(combB)

  override def setUp() {
    super.setUp()
    val panel = new DamageEffectEditor(tracker)
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

  protected def setSourceAndTarget(source: UnifiedCombatantID, target: UnifiedCombatantID) {
    view.changeSourceContext(Some(source))
    view.changeTargetContext(Some(target))
  }

  protected def pickDuration(durationDescription: String): DurationComboEntry =
    DurationComboEntry.durations.find(_.toString == durationDescription).get

}

trait DamageEffectEditorFieldSelector {
  self: UISpecTestCase =>

  protected def getConditionField = getMainWindow.getTextBox("dee.condition")

  protected def getNameField = getMainWindow.getTextBox("dee.name")

  protected def getDamageField = getMainWindow.getTextBox("dee.damage")

  protected def getDamageValueField = getMainWindow.getTextBox("dee.damageValue")

  protected def getRollButton = getMainWindow.getButton("dee.roll")

  protected def getMarkCheckbox = getMainWindow.getCheckBox("dee.mark")

  protected def getPermanentMarkCheckbox = getMainWindow.getCheckBox("dee.permanentMark")

  protected def getDurationCombo = getMainWindow.getComboBox("dee.duration")

  protected def getApplyButton = getMainWindow.getButton("dee.apply")

}

