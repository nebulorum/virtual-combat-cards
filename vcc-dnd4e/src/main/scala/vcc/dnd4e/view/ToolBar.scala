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

import vcc.dnd4e.tracker.common.Command.ExecuteInitiativeAction

import scala.swing.{Label, Action, Button}
import vcc.dnd4e.tracker.common._
import vcc.util.swing.MigPanel

class ToolBar(director: PanelDirector) extends MigPanel("flowx, ins 2", "[]rel[]100[][][]unrel[]push[]", "[]")
with CombatStateObserver with ContextObserver {

  private var state = new UnifiedSequenceTable(Array(), CombatState.empty)
  private var targetId: Option[UnifiedCombatantID] = None

  private val nextButton = createButton("toolbar.next", makeFirstAction("Next turn", InitiativeAction.EndRound))
  private val delayButton = createButton("toolbar.delay", makeFirstAction("Delay turn", InitiativeAction.DelayAction))
  private val readyButton = createButton("toolbar.ready", makeFirstAction("Ready action", InitiativeAction.ReadyAction))
  private val executeButton = createButton("toolbar.executeReady", makeTargetAction("Execute action", InitiativeAction.ExecuteReady))
  private val activeCombo = makeSourceCombo()

  add(new Label("Acting:"))
  add(activeCombo)
  add(nextButton)
  add(delayButton)
  add(readyButton)
  add(executeButton)

  private def makeSourceCombo() = {
    val combo = new SourceCombatantCombo(director)
    combo.name = "toolbar.activeCombo"
    combo
  }

  def combatStateChanged(newState: UnifiedSequenceTable) {
    state = newState
    activeCombo.combatStateChanged(newState)
    adjustControls()
  }

  override def changeTargetContext(newContext: Option[UnifiedCombatantID]) {
    targetId = newContext
    adjustControls()
  }

  override def changeSourceContext(newContext: Option[UnifiedCombatantID]) {
    activeCombo.changeSourceContext(newContext)
  }

  private def adjustControls() {
    nextButton.enabled = state.orderFirst.isDefined

    val orderId = state.orderFirstId.map(_.orderId.toLabelString)
    val target = targetId.map(_.toLabelString)
    adjustButton(delayButton, orderId, x => s"[$x] Delay turn", "Delay turn")
    adjustButton(readyButton, orderId, x => s"[$x] Ready action", "Ready action")
    adjustTargetButton(executeButton, target,
      enabled = isInOrderAndReady(targetId),
      x => s"[$x] Execute ready ", "Execute ready")
  }

  private def isInOrderAndReady(id: Option[UnifiedCombatantID]) =
    state.combatantOption(id).exists(c => c.isInOrder && c.initiative.state == InitiativeState.Ready)

  private def adjustButton(button: Button, key: Option[String], definedFormatter: String => String, default: String) {
    if (key.isDefined) {
      button.enabled = true
      button.text = definedFormatter(key.get)
    } else {
      button.enabled = false
      button.text = default
    }
  }

  private def adjustTargetButton(button: Button, key: Option[String], enabled: Boolean, definedFormatter: String => String, default: String) {
    if (key.isDefined) {
      button.text = definedFormatter(key.get)
    } else {
      button.text = default
    }
    button.enabled = enabled
  }

  private def createButton(name: String, action: Action) = {
    val button = new Button(action)
    button.name = name
    button
  }

  private def makeFirstAction(text: String, action: InitiativeAction.Value) =
    Action(text) {
      director.requestAction(ExecuteInitiativeAction(state.orderFirstId.get.orderId, action))
    }

  private def makeTargetAction(text: String, action: InitiativeAction.Value) =
    Action(text) {
      director.requestAction(ExecuteInitiativeAction(targetId.get.orderId, action))
    }

}