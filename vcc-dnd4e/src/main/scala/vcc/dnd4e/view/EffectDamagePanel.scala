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

import vcc.util.swing.MigPanel
import vcc.infra.docking.{ScalaDockableComponent, DockID}
import vcc.dnd4e.tracker.common.{CombatState, UnifiedCombatantID, UnifiedSequenceTable}
import vcc.dnd4e.view.DamageEffectEditor.Memento

class EffectDamagePanel(director: PanelDirector)
  extends MigPanel("") with ScalaDockableComponent with ContextObserver with CombatStateObserver {

  private val sourceCombo = makeSourceCombo()
  private val damageEffectEditor = new DamageEffectEditor(director)
  private val groupForm = new GroupFormPanel[Memento](damageEffectEditor, _.asListText)

  private var state = new UnifiedSequenceTable(Array(), CombatState.empty)
  private var currentSource: Option[UnifiedCombatantID] = None
  private val mementoCache = scala.collection.mutable.Map[String, Seq[Memento]]()

  add(sourceCombo, "wrap")
  add(groupForm, "wrap")

  override def dockFocusComponent = sourceCombo.peer

  override def dockID = DockID("EffectDamage")

  override def dockTitle = "Effect and Damage"

  override def combatStateChanged(newState: UnifiedSequenceTable) {
    state = newState
    sourceCombo.combatStateChanged(newState)
  }

  override def changeSourceContext(newContext: Option[UnifiedCombatantID]) {
    swapCachedContent(newContext)
    sourceCombo.changeSourceContext(newContext)
    currentSource = newContext
    damageEffectEditor.changeSourceContext(newContext)
  }

  override def changeTargetContext(newContext: Option[UnifiedCombatantID]) {
    sourceCombo.changeTargetContext(newContext)
    damageEffectEditor.changeTargetContext(newContext)
  }

  private def makeSourceCombo() = {
    val combo = new SourceCombatantCombo(director)
    combo.name = "edp.source"
    combo
  }

  private def swapCachedContent(newContext: Option[UnifiedCombatantID]) {
    val oldEntityID = state.combatantOption(currentSource).map(_.definition.entity.eid)
    val newEntityID = state.combatantOption(newContext).map(_.definition.entity.eid)
    if (oldEntityID != newEntityID) {
      oldEntityID.foreach(mementoCache.update(_, groupForm.getContent))
      newEntityID.foreach(eid => groupForm.setContent(mementoCache.get(eid).getOrElse(Seq())))
    }
  }
}