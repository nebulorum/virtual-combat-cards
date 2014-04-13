/*
 * Copyright (C) 2014-2014 - Thomas Santana <tms@exnebula.org>
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

import vcc.dnd4e.tracker.common.{UnifiedCombatantID, UnifiedCombatant, UnifiedSequenceTable}
import vcc.util.swing.{StringFormatListCellRenderer, ContainerComboBoxModel, ExplicitModelComboBox}
import scala.swing.event.SelectionChanged

class SourceCombatantCombo private(director: PanelDirector, mdl: ContainerComboBoxModel[UnifiedCombatant])
  extends ExplicitModelComboBox[UnifiedCombatant](mdl) with CombatStateObserver with ContextObserver {

  def this(director: PanelDirector) = this(director, new ContainerComboBoxModel[UnifiedCombatant](Nil))

  private var changing = false

  listenTo(selection)
  setFormatRenderer(new StringFormatListCellRenderer[UnifiedCombatant]({
    combatant =>
      (if (combatant.isInOrder) combatant.orderId.toLabelString else combatant.combId.id) + " - " + combatant.name
  }))

  reactions += {
    case SelectionChanged(_) if !changing =>
      director.setActiveCombatant(Option(selection.item.unifiedId))
  }

  def combatStateChanged(newState: UnifiedSequenceTable) {
    mdl.contents = newState.elements
  }

  override def changeSourceContext(newContext: Option[UnifiedCombatantID]) {
    if (!mdl.contents.isEmpty) {
      changing = true
      val item = newContext.flatMap(ctx => mdl.contents.find(_.matches(ctx)))
      selection.item = item.getOrElse(mdl.contents.head)
      repaint()
      changing = false
    }
  }
}