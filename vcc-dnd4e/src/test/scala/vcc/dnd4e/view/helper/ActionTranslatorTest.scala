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
//$Id$
package vcc.dnd4e.view.helper

import org.specs.SpecificationWithJUnit
import org.specs.mock.Mockito
import vcc.dnd4e.domain.tracker.common.Command.{InternalInitiativeAction}
import vcc.dnd4e.domain.tracker.common.InitiativeTracker._
import vcc.controller.message.TransactionalAction
import vcc.dnd4e.domain.tracker.common._
import vcc.dnd4e.model.CombatantEntity
import vcc.dnd4e.model.common.CombatantType

class ActionTranslatorTest extends SpecificationWithJUnit with Mockito {

  val combA = CombatantID("A")
  val combB = CombatantID("B")
  val ioiA = InitiativeOrderID(combA, 0)
  val ioiB = InitiativeOrderID(combB, 0)
  val mState = mock[CombatStateView]
  val mCombA = mock[CombatantStateView]
  val mCombB = mock[CombatantStateView]
  mState.combatantViewFromID(combA) returns mCombA
  mState.combatantViewFromID(combB) returns mCombB
  val combADef = combatantDefinition(combA, "Goblin", "Shorty", CombatantType.Monster)
  val combBDef = combatantDefinition(combB, "Goblin", null, CombatantType.Monster)
  mCombA.definition returns combADef
  mCombB.definition returns combBDef

  "ActionTranslator" should {
    "fallback to description" in {
      val act = mock[TransactionalAction]
      val desc = "something " + System.currentTimeMillis
      act.description returns desc
      ActionTranslator.fullActionMessage(mState, act) must_== desc
    }

    "tranlate StartRound with alias" in {
      ActionTranslator.fullActionMessage(mState, InternalInitiativeAction(ioiA, action.StartRound)) must_== "Start round of Goblin - Shorty [Aº]"
    }

    "tranlate StartRound without alias" in {
      ActionTranslator.fullActionMessage(mState, InternalInitiativeAction(ioiB, action.StartRound)) must_== "Start round of Goblin [Bº]"
    }

    "tranlate EndRound with alias" in {
      ActionTranslator.fullActionMessage(mState, InternalInitiativeAction(ioiA, action.EndRound)) must_== "End round of Goblin - Shorty [Aº]"
    }

    "tranlate EndRound without alias" in {
      ActionTranslator.fullActionMessage(mState, InternalInitiativeAction(ioiB, action.EndRound)) must_== "End round of Goblin [Bº]"
    }
  }

  def combatantDefinition(comb: CombatantID, name: String, alias: String, ctype: CombatantType.Value) =
    CombatantRosterDefinition(comb, alias, CombatantEntity(null, name, null, 0, ctype, null))

}