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

import org.mockito.Mockito._
import org.uispec4j.{Button, UISpecAdapter, UISpecTestCase, Window}
import vcc.dnd4e.tracker.command._
import vcc.dnd4e.tracker.common.Command.ExecuteInitiativeAction
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.event.StartCombatEvent

import scala.swing.Frame

class ToolBarTest extends UISpecTestCase with CombatStateBuilder {

  private var view: ToolBar = null
  private val roster = Seq(
    (Some(combA), null, entityFighter),
    (None, null, entityGoblin),
    (None, null, entityGoblin),
    (None, null, entityMinion))

  private val director = mock(classOf[PanelDirector])
  private val combatantNames = Seq("Aº - Fighter", "1º - Goblin", "2º - Goblin", "3 - Goblin-mini")

  override def setUp() {
    super.setUp()
    val panel = new ToolBar(director)
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

  def testSource_componentPresentAndEnabled() {
    assertThat(getSourceCombo.isVisible)
    assertThat(getSourceCombo.isEnabled)
  }

  def testSource_FromOutside_shouldUpdateCombo() {
    loadRoster(combatStartedState)
    view.changeSourceContext(pickCombatant(combatStartedState, 1))

    assertThat(getSourceCombo.contains(combatantNames: _*))
    assertThat(getSourceCombo.selectionEquals(combatantNames(1)))
  }

  def testSource_selectingDifferentSource_updatesPanel() {
    loadRoster(combatStartedState)
    getSourceCombo.select(combatantNames(2))

    verify(director).setActiveCombatant(pickCombatant(combatStartedState, 2))
  }

  def testNext_RosterLoad_nextNotEnabled() {
    loadRoster(initialState)

    assertButtonDisabledWithText(getNextButton, "Next turn")
  }

  def testNext_CombatStarted_nextEnabled() {
    loadRoster(combatStartedState)

    assertTrue(getNextButton.isEnabled)
  }

  def testNext_SetAndClear_nextIsDisabled() {
    loadRoster(combatStartedState)
    loadRoster(initialState)

    assertThat(not(getNextButton.isEnabled))
  }

  def testNext_CombatStartedClick_sendEvents() {
    loadRoster(combatStartedState)

    getNextButton.click()

    verify(director).requestAction(ExecuteInitiativeAction(combatStartedState.nextUp.get, InitiativeAction.EndRound))
  }

  def testDelay_CombatStarted_delayEnabledAndAdjusted() {
    loadRoster(combatStartedState)

    assertButtonActiveWithText(getDelayButton, "[Aº] Delay turn")
  }

  def testDelay_RosterLoad_delayDisabled() {
    loadRoster(initialState)

    assertButtonDisabledWithText(getDelayButton, "Delay turn")
  }

  def testDelay_RosterLoadAndClear_delayButtonReset() {
    loadRoster(combatStartedState)
    loadRoster(initialState)

    assertButtonDisabledWithText(getDelayButton, "Delay turn")
  }

  def testDelay_CombatStartedClickDelay_dispatchDelayEvent() {
    loadRoster(combatStartedState)
    getDelayButton.click()

    verify(director).requestAction(ExecuteInitiativeAction(combatStartedState.nextUp.get, InitiativeAction.DelayAction))
  }

  def testReady_CombatStarted_readyEnabledAndAdjusted() {
    loadRoster(combatStartedState)

    assertButtonActiveWithText(getReadyButton, "[Aº] Ready action")
  }

  def testReady_RosterLoad_readyDisabled() {
    loadRoster(initialState)

    assertButtonDisabledWithText(getReadyButton, "Ready action")
  }

  def testReady_RosterLoadAndClear_readyButtonReset() {
    loadRoster(combatStartedState)
    loadRoster(initialState)

    assertButtonDisabledWithText(getReadyButton, "Ready action")
  }

  def testReady_CombatStartedClickDelay_dispatchReadyAction() {
    loadRoster(combatStartedState)
    getReadyButton.click()

    verify(director).requestAction(ExecuteInitiativeAction(combatStartedState.nextUp.get, InitiativeAction.ReadyAction))
  }

  def testExecuteReady_noTarget_doesNotEnableButton() {
    loadRoster(readiedCombatant)

    view.changeTargetContext(None)

    assertButtonDisabledWithText(getExecuteButton, "Execute ready")
  }

  def testExecuteReady_selectNotReady_doesNotEnableButton() {
    loadRoster(readiedCombatant)
    view.changeTargetContext(pickCombatant(readiedCombatant, 1))

    assertButtonDisabledWithText(getExecuteButton, "[2º] Execute ready")
  }

  def testExecuteReady_selectNotInOrder_doesNotEnableButton() {
    loadRoster(readiedCombatant)
    view.changeTargetContext(pickCombatant(readiedCombatant, 3))

    assertButtonDisabledWithText(getExecuteButton, "[3] Execute ready")
  }

  def testExecuteReady_selectReady_enableButton() {
    loadRoster(readiedCombatant)
    view.changeTargetContext(pickCombatant(readiedCombatant, 2))

    assertButtonActiveWithText(getExecuteButton, "[Aº] Execute ready")
  }

  def testExecuteReady_selectReadyAndClick_triggersEvent() {
    loadRoster(readiedCombatant)
    view.changeTargetContext(pickCombatant(readiedCombatant, 2))

    getExecuteButton.click()

    verify(director).requestAction(ExecuteInitiativeAction(ioiA, InitiativeAction.ExecuteReady))
  }

  private def getDelayButton = getMainWindow.getButton("toolbar.delay")

  private def getNextButton = getMainWindow.getButton("toolbar.next")

  private def getReadyButton = getMainWindow.getButton("toolbar.ready")

  private def getExecuteButton = getMainWindow.getButton("toolbar.executeReady")

  private def getSourceCombo = getMainWindow.getComboBox("toolbar.activeCombo")

  private def assertButtonActiveWithText(button: Button, expectedText: String) {
    assertThat(button.isEnabled)
    assertThat(button.textEquals(expectedText))
  }

  private def assertButtonDisabledWithText(button: Button, expectedText: String) {
    assertThat(button.textEquals(expectedText))
    assertThat("Button should be disabled", not(button.isEnabled))
  }

  private def pickCombatant(state: CombatState, index : Int) = {
    val us = buildUnifiedState(state)
    Some(us.elements(index).unifiedId)
  }

  private val builder = new UnifiedSequenceTable.Builder

  private def loadRoster(state: CombatState) {
    view.combatStateChanged(buildUnifiedState(state))
  }

  private val initialState = buildState(buildRoster(roster: _*),
    addToOrder(comb1, 10, 2),
    addToOrder(combA, 15, 2),
    addToOrder(comb2, 5, 2))

  private val ioiA = InitiativeOrderID(combA, 0)
  private val ioi1 = InitiativeOrderID(comb1, 0)
  private val combatStartedState = buildState(initialState, StartCombatEvent)

  private def buildFromCommands(state: CombatState, events: Seq[CombatStateCommand]) =
    events.foldLeft(state)((s,cmd) => s.transitionWith(cmd.generateEvents(s)))

  private val readiedCombatant = buildFromCommands(combatStartedState,
    Seq(StartRoundCommand(ioiA), ReadyActionCommand(ioiA), EndRoundCommand(ioiA), StartRoundCommand(ioi1)))

  private def buildUnifiedState(state: CombatState) = builder.build(state)
}