/*
 * Copyright (C) 2013-2013 - Thomas Santana <tms@exnebula.org>
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

import vcc.dnd4e.tracker.common.CombatState
import vcc.dnd4e.tracker.event.EventSourceSampleEvents
import vcc.tracker.Ruling
import vcc.dnd4e.tracker.command.AlterDamageIndicationCommand

class AlterDamageRulingTest extends RulingAcceptance[CombatState]("OngoingDamageRuling") with EventSourceSampleEvents {
  import vcc.dnd4e.tracker.event.AlterDamageIndicationEvent._
  protected val state = CombatState.empty.transitionWith(List(evtAddCombA, makeBadEndOfEncounterEffect(combA, combB, "ongoing 5 fire")))
  protected val rulingWithAnswer: Ruling[CombatState, _, _] = AlterDamageRuling("Some condition", Reduce(5), Some(true))
  protected val rulingWithNegativeAnswer: Ruling[CombatState, _, _] = AlterDamageRuling("Some condition", Reduce(5), Some(false))
  protected val rulingWithoutAnswer: Ruling[CombatState, _, _] = AlterDamageRuling("Some condition", Reduce(5), None)
  protected val userPromptMessage: String = "Some condition"

  def buildCases = s2"""
    negative decision will produce no events ${
      rulingWithNegativeAnswer.generateCommands(state) must_== Nil
    }
    positive answer should generate  ${
      rulingWithAnswer.generateCommands(state) must_== List(AlterDamageIndicationCommand(Reduce(5)))
    }
    """
}