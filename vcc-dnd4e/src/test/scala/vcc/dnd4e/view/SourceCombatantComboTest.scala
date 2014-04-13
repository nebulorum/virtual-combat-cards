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

import org.uispec4j.{Window, UISpecAdapter, UISpecTestCase}
import scala.swing.Frame
import vcc.dnd4e.tracker.common.{UnifiedCombatantID, CombatState, CombatStateBuilder, UnifiedSequenceTable}
import org.mockito.Mockito._

class SourceCombatantComboTest extends UISpecTestCase with CombatStateBuilder {

  private val uc2 = UnifiedCombatantID(comb2)

  private val builder = new UnifiedSequenceTable.Builder
  private var combo: SourceCombatantCombo = null
  private val director = mock(classOf[PanelDirector])
  private val roster = Seq(
    (Some(combA), null, entityFighter),
    (None, null, entityGoblin),
    (None, null, entityGoblin),
    (None, null, entityGoblin))

  override def setUp() {
    super.setUp()
    combo = new SourceCombatantCombo(director)
    setAdapter(new UISpecAdapter() {
      def getMainWindow: Window = {
        val frame = new Frame {
          contents = combo
        }
        new Window(frame.peer)
      }
    })
  }

  def testChangingState_shouldUpdateCombo() {
    combo.combatStateChanged(buildUnifiedState(buildRoster(roster.head)))
    assertThat(getCombo.contains("A - Fighter"))
  }

  def testSetContextToNoneMustWorkOnEmptyList() {
    combo.changeSourceContext(None)
  }

  def testChangingStateInOrder_shouldUpdateCombo() {
    combo.combatStateChanged(buildUnifiedState(buildState(buildRoster(roster.head), addToOrder(combA, 1, 2))))
    assertThat(getCombo.contains("Aº - Fighter"))
  }

  def testChangingState_withSeveral() {
    combo.combatStateChanged(buildUnifiedState(buildRoster(roster.init: _*)))
    assertThat(getCombo.contains("A - Fighter", "1 - Goblin", "2 - Goblin"))
  }

  def testSelecting_willSelectSourceAndPropagate() {
    combo.combatStateChanged(buildUnifiedState(buildRoster(roster.init: _*)))
    getCombo.select("2 - Goblin")
    assertThat(getCombo.selectionEquals("2 - Goblin"))
    verify(director).setActiveCombatant(Some(uc2))
  }

  def testChangingState_preservesSelection() {
    combo.combatStateChanged(buildUnifiedState(buildRoster(roster.tail ++ roster.init: _*)))
    getCombo.select("2 - Goblin")
    verify(director).setActiveCombatant(Some(uc2))

    loadRoster()
    assertThat(getCombo.selectionEquals("2 - Goblin"))
    verifyNoMoreInteractions(director)
  }

  def testAfterInitialLoadExternallyChangingSelection_shouldNotPropagate() {
    loadRoster()
    combo.changeSourceContext(Some(uc2))
    assertThat(getCombo.selectionEquals("2 - Goblin"))
  }

  def testChangeContextToNone_shouldClearSelection() {
    loadRoster()
    getCombo.select("2 - Goblin")
    reset(director)
    combo.changeSourceContext(None)
    assertThat(getCombo.selectionEquals("Aº - Fighter"))
    verifyNoMoreInteractions(director)

  }

  private def loadRoster() {
    combo.combatStateChanged(buildUnifiedState(
      buildState(buildRoster(roster: _*),
        addToOrder(combA, 1, 2))))
  }

  private def buildUnifiedState(state: CombatState) = builder.build(state)

  private def getCombo = getMainWindow.getComboBox
}