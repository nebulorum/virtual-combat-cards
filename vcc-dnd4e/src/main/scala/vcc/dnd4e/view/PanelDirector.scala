/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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

import ruling.NextUpRulingDialog
import vcc.dnd4e.domain.tracker.common._
import vcc.util.swing.SwingHelper
import vcc.dnd4e.tracker.common.CombatState
import vcc.dnd4e.tracker.common.Command.CombatStateAction
import vcc.dnd4e.tracker.dispatcher.CombatStateViewAdapterBuilder
import vcc.dnd4e.tracker.ruling.{NextUpRuling, AutomaticRulingProvider}
import vcc.dnd4e.tracker.command.NextUpCommand
import vcc.tracker.{RulingContext, Ruling, RulingProvider, Tracker}

trait ContextObserver {
  def changeTargetContext(newContext: Option[UnifiedCombatantID]) {}

  def changeSourceContext(newContext: Option[UnifiedCombatantID]) {}
}

object PanelDirector {

  object property extends Enumeration {
    val HideDead = Value("Hide Dead")
    val RobinView = Value("Robin View")
  }

}

trait CombatStateObserver {
  def combatStateChanged(newState: UnifiedSequenceTable)
}

/**
 * This Mixin is used to help build panels that need to cache and
 * update combatState but that are not interested in the changes or have
 * to react to the change. If a Panel needs to update as soon as state changes
 * do not use this mixin.
 */
trait SimpleCombatStateObserver extends CombatStateObserver {
  protected var combatState: UnifiedSequenceTable = null

  def combatStateChanged(newState: UnifiedSequenceTable) {
    combatState = newState
  }
}

/**
 * This component act as a Mediator between all the panels and the CombatStateManager.
 */
class PanelDirector(tracker: Tracker[CombatState], statusBar: StatusBar) {
  private var combatStateObserver: List[CombatStateObserver] = Nil
  private var contextObserver: List[ContextObserver] = Nil
  private var propHideDead = false
  private var propRobinView = true
  private var unifiedTable: UnifiedSequenceTable = null
  private val sequenceBuilder = new UnifiedSequenceTable.Builder()

  private object RulingBroker extends RulingProvider[CombatState] {
    private val automatic = new AutomaticRulingProvider


    def provideRulingFor(context: RulingContext[CombatState]): List[Ruling[CombatState, _, _]] = {
      context.rulingNeedingDecision match {
        case nur@NextUpRuling(next, _) :: Nil => askForNextUp(context.state, next)
        case other => automatic.provideRulingFor(context)
      }
    }

    private def askForNextUp(state: CombatState, next: NextUpCommand): scala.List[NextUpRuling] = {
      val dialog = new NextUpRulingDialog(null, state, NextUpRuling(next, None))
      val result = dialog.promptUser()
      dialog.peer.dispose()
      result.toList
    }

  }

  val rules = new CombatStateRules()

  private val logger = org.slf4j.LoggerFactory.getLogger("user")

  tracker.addObserver(new Tracker.Observer[CombatState] {
    def stateUpdated(state: CombatState) {
      logger.debug("[NT] -> " + state)
      val newState = CombatStateViewAdapterBuilder.buildView(state)
      buildStateAndPropagate(newState)
    }
  })

  def registerContextObserver(obs: ContextObserver) {
    contextObserver = obs :: contextObserver
  }

  def registerStateObserver(obs: CombatStateObserver) {
    combatStateObserver = obs :: combatStateObserver
  }

  def setActiveCombatant(id: Option[UnifiedCombatantID]) {
    contextObserver.foreach(obs => obs.changeSourceContext(id))
  }

  def setTargetCombatant(id: Option[UnifiedCombatantID]) {
    contextObserver.foreach(obs => obs.changeTargetContext(id))
  }

  def setStatusBarMessage(text: String) {
    statusBar.setTipText(text)
  }

  def setProperty(property: PanelDirector.property.Value, value: Boolean) {
    property match {
      case PanelDirector.property.HideDead =>
        propHideDead = value
        if (value) sequenceBuilder.hideDead()
        else sequenceBuilder.showDead()
      case PanelDirector.property.RobinView =>
        propRobinView = value
        if (value) sequenceBuilder.useRobinOrder()
        else sequenceBuilder.useDirectOrder()
      case _ =>
        throw new Exception("Unknown property: " + property)
    }
    buildStateAndPropagate(unifiedTable.state)
  }

  def getBooleanProperty(prop: PanelDirector.property.Value): Boolean = {
    import PanelDirector.property._
    prop match {
      case HideDead => propHideDead
      case RobinView => propRobinView
      case _ => throw new Exception("Unknown property: " + prop)
    }
  }

  /**
   * This is the way to request tracker actions. No Panel should
   * send messages directly to the Tracker.
   * @param action A TransactionalAction message
   */
  def requestAction(action: CombatStateAction) {
    tracker.dispatchAction(action, RulingBroker)
  }

  def requestClearHistory() {
    tracker.clearHistory()
  }

  def requestUndo() {
    tracker.undo()
  }

  def requestRedo() {
    tracker.redo()
  }

  def actionCompleted(msg: String, producedChanges: Boolean) {
    logger.info("Executed: " + msg)
  }

  def actionCancelled(reason: String) {
    logger.warn("Failed to execute action, reason: " + reason)
  }

  private def buildStateAndPropagate(newState: CombatStateView) {
    SwingHelper.invokeInEventDispatchThread {
      unifiedTable = sequenceBuilder.build(newState)
      combatStateObserver.foreach(obs => obs.combatStateChanged(unifiedTable))
    }
  }

}