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

import vcc.dnd4e.tracker.command.HealCommand
import vcc.dnd4e.tracker.event.{EventSourceSampleEvents}
import vcc.tracker.Ruling
import vcc.dnd4e.tracker.common.{CombatState, EffectID}

class RegenerationRulingTest extends RulingAcceptance[CombatState]("OngoingDamageRuling") with EventSourceSampleEvents {
  private val eid = EffectID(combA, 1)
  private val zeroRegenRuling = RegenerationRuling(eid, Some(0))
  private val realRegenRuling = RegenerationRuling(eid, Some(7))

  protected val state = emptyState.transitionWith(List(evtAddCombA, makeBadEndOfEncounterEffect(combA, combB, "regenerate 5")))
  protected val rulingWithAnswer: Ruling[CombatState, _, _] = zeroRegenRuling
  protected val rulingWithoutAnswer: Ruling[CombatState, _, _] = RegenerationRuling(eid, None)
  protected val userPromptMessage: String = "Fighter [A] affected by: regenerate 5"

  def buildCases =
    "produce command no command when zero" ! (zeroRegenRuling.generateCommands(state) must_== Nil) ^
      "produce heal command when set to greater than 0" !
        (realRegenRuling.generateCommands(state) must_== List(HealCommand(combA, 7))) ^
      end
}