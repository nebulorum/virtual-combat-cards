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
package vcc.dnd4e.tracker.ruling

import vcc.tracker.{InvalidDecisionException, Ruling}
import vcc.dnd4e.tracker.event.EventSourceSampleEvents
import vcc.dnd4e.tracker.common.{InitiativeState, CombatState, SampleStateData}
import vcc.dnd4e.tracker.command.{EndRoundCommand, NextUpCommand, MoveUpCommand, StartRoundCommand}

class NextUpRulingTest extends RulingAcceptance[CombatState]("NextUpRuling") with EventSourceSampleEvents {

  private val ruling = NextUpRuling(NextUpCommand(ioA0, List(io1_0, ioB0)), None)

  protected val state = CombatState.empty.transitionWith(List(
    evtAddCombA, evtAddCombNoId, evtAddCombB, meAddToOrder(combA, 1, 10), evtStart))

  protected val state2 = state.transitionWith(List(ForceChangeEvent(s => s.lensFactory.initiativeTrackerLens(ioA0).mod(state,_.copy(state = InitiativeState.Delaying)))))
  protected val rulingWithAnswer: Ruling[CombatState, _, _] = ruling.withDecision(io1_0)
  protected val rulingWithoutAnswer: Ruling[CombatState, _, _] = ruling
  protected val userPromptMessage: String = "Select which combatant should act next"

  println(state2.order.tracker)

  def buildCases =
    "generate StartRount if main selected" !
      (ruling.withDecision(ioA0).generateCommands(state) must_== List(StartRoundCommand(ioA0))) ^
      "generate EndRound and StartRound if main selected but delaying" !
        (ruling.withDecision(ioA0).generateCommands(state2) must_== List(EndRoundCommand(ioA0), StartRoundCommand(ioA0))) ^
      "generate moveUp if delaying is selected" !
        (ruling.withDecision(io1_0).generateCommands(state) must_== List(MoveUpCommand(io1_0))) ^
      "generate moveUp if delaying is selected " !
        (ruling.withDecision(ioB0).generateCommands(state) must_== List(MoveUpCommand(ioB0))) ^
      "not allow answers " !
        (ruling.withDecision(io2_0) must throwA(new InvalidDecisionException(io2_0 + " is not eligible to act")))
}