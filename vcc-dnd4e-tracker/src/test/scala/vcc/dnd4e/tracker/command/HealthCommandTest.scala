/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.tracker.command

import org.specs2.SpecificationWithJUnit
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.event._
import vcc.tracker.IllegalActionException

class HealthCommandTest extends SpecificationWithJUnit with CombatStateEventSourceBehavior with EventSourceSampleEvents {
  def is = {
    "Health change command".title ^
      damaging ^
      runTestFor(cid => (HealCommand(cid, 10), ApplyHealingEvent(cid, 10))) ^
      runTestFor(cid => (SetTemporaryHPCommand(cid, 10), SetTemporaryHitPointsEvent(cid, 10))) ^
      runTestFor(cid => (FailDeathSaveCommand(cid), FailDeathSaveEvent(cid))) ^
      runTestFor(cid => (RevertDeathCommand(cid), RevertDeathEvent(cid))) ^
      end
  }

  def runTestFor(builder: CombatantID => (CombatStateCommand, CombatStateEvent)) = {
    val (cmd, evt) = builder(combA)
    ("Command " + cmd.getClass.getSimpleName) ^
      "fail if not found" !
        (given(emptyState) when cmd failWith notFoundException) ^
      "generate event" !
        (given(emptyState, evtAddCombA) when cmd andThen evt) ^
      endp

  }

  private val notFoundException = new IllegalActionException("Combatant " + combA + " not in combat")
  private val illegalStateException = new IllegalStateException("No damage indication present")

  private def damaging =
    s2"""command ApplyDamageTransition
    not insert indication if target not present ${
      given(emptyState, evtAddCombNoId) when AddDamageIndicationCommand(combA, 10) failWith notFoundException
    }
    insert damage indication into state ${
      given(emptyState, evtAddCombA, evtAddCombNoId, evtAddCombNoId, evtInitA, meAddToOrder(comb1, 3, 3), evtStart).
        when(AddDamageIndicationCommand(combA, 10)).
        andThen(SetDamageIndicationEvent(combA, 10))
    }
    fail if applying but no indication is present in state ${
        given(emptyState, evtAddCombNoId).
          when(ApplyDamageCommand).
          failWith(illegalStateException)
    }
    simple damage ${
        given(emptyState, evtAddCombA, evtAddCombNoId, evtAddCombNoId, evtInitA,
          meAddToOrder(comb1, 3, 3), evtStart, SetDamageIndicationEvent(combA, 10)).
          when(ApplyDamageCommand).
          andThen(ApplyDamageEvent(combA, 10), ClearDamageIndicationEvent)
    }
    damage and end combat when last acting is killed ${
        given(emptyState, evtAddCombA, evtAddCombNoId, evtAddCombNoId, evtInitA,
          meAddToOrder(comb1, 3, 3), evtStart, killEvent(comb1), SetDamageIndicationEvent(combA, 1000)).
          when(ApplyDamageCommand).
          andThen(ApplyDamageEvent(combA, 1000), ClearDamageIndicationEvent, evtEnd)
    }
    killing but not the last live combatant does not end combat ${
        given(emptyState, evtAddCombA, evtAddCombNoId, evtAddCombNoId, evtInitA,
          meAddToOrder(comb1, 3, 3), evtStart, SetDamageIndicationEvent(combA, 1000)).
          when(ApplyDamageCommand).
          andThen(ApplyDamageEvent(combA, 1000), ClearDamageIndicationEvent)
    }"""

}