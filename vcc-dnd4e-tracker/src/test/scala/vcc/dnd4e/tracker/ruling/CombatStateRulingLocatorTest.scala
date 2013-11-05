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
package vcc.dnd4e.tracker.ruling

import org.specs2.SpecificationWithJUnit
import vcc.dnd4e.tracker.common.Effect.Condition
import vcc.dnd4e.tracker.common.{EffectID, Duration, CombatState}
import vcc.dnd4e.tracker.event.{SetDamageIndicationEvent, ApplyDamageEvent, AddEffectEvent, EventSourceSampleEvents}
import vcc.dnd4e.tracker.command.{ApplyDamageCommand, NextUpCommand, StartRoundCommand, EndRoundCommand}
import vcc.tracker.Ruling
import vcc.dnd4e.tracker.event.AlterDamageIndicationEvent.Reduce

class CombatStateRulingLocatorTest extends SpecificationWithJUnit with EventSourceSampleEvents {

  private val durationSaveEnd = Duration.SaveEnd
  private val durationSaveEndSpecial = Duration.SaveEndSpecial
  private val durationEoTSA = Duration.RoundBound(ioA0, Duration.Limit.EndOfTurnSustain)
  private val aCondition = Condition.Generic("bad", beneficial = false)
  private val anotherCondition = Condition.Generic("bad -> worst", beneficial = false)
  private val regenerateCondition = Condition.Generic("regenerate 10 while bloodied", beneficial = true)
  private val ongoingCondition = Condition.Generic("ongoing 5 fire", beneficial = false)
  private val resistDamage = Condition.Generic("resist 5 fire", beneficial = true)

  private val state = CombatState.empty.transitionWith(List(
    evtAddCombA, evtAddCombNoId, evtInitA, evtStart,
    AddEffectEvent(combA, combA, aCondition, durationSaveEnd),
    AddEffectEvent(combA, combA, anotherCondition, durationSaveEndSpecial),
    AddEffectEvent(comb1, combA, aCondition, durationEoTSA),
    AddEffectEvent(combA, combA, aCondition, durationEoTSA),
    AddEffectEvent(combA, combA, ongoingCondition, durationSaveEnd),
    AddEffectEvent(combA, combA, regenerateCondition, durationSaveEnd),
    AddEffectEvent(combA, combA, resistDamage, durationSaveEnd),
    SetDamageIndicationEvent(combA, 10)
  ))

  private val eidA1 = EffectID(combA, 1)
  private val eidA2 = EffectID(combA, 2)
  private val eidA3 = EffectID(combA, 3)
  private val eidA4 = EffectID(combA, 4)
  private val eidA5 = EffectID(combA, 5)
  private val eid1_1 = EffectID(comb1, 1)

  def is = s2"""
  EndRoundCommand should
    detect Save on end of round ${
      EndRoundCommand(ioA0).requiredRulings(state) must contain(SaveRuling(eidA1, None))
    }
    detect Save Special on end of round ${
      EndRoundCommand(ioA0).requiredRulings(state) must contain(SaveSpecialRuling(eidA2, None))
    }
    detect all sustains on end of round ${
      (EndRoundCommand(ioA0).requiredRulings(state)
        must contain(allOf[Ruling[CombatState, _, _]](SustainEffectRuling(eidA3, None), SustainEffectRuling(eid1_1, None))))
    }
    detect save versus death when appropriate ${
      val nState = state.transitionWith(List(ApplyDamageEvent(combA, 41)))
      (EndRoundCommand(ioA0).requiredRulings(nState)
        must contain(SaveVersusDeathRuling(combA, None)))
    }

  StartRoundCommand should
    detect Ongoing damage ${
      (StartRoundCommand(ioA0).requiredRulings(state)
        must contain(OngoingDamageRuling(eidA4, None)))
    }
    detect Regeneration damage ${
      (StartRoundCommand(ioA0).requiredRulings(state)
        must contain(RegenerationRuling(eidA5, None)))
    }
    detect regeneration first ${
      val regen = List(RegenerationRuling(eidA5, None), OngoingDamageRuling(eidA4, None))
      val detected = StartRoundCommand(ioA0).requiredRulings(state)
      (detected must contain(regen(0))) and
        (detected must contain(regen(1))) and
        (detected.indexOf(regen(0)) must beLessThan(detected.indexOf(regen(1))))
    }

  NextUpCommand should
    ask for a decision on what to put first ${
      val nextUpCommand = NextUpCommand(io1_0, List(io2_0, ioA0))
      nextUpCommand.requiredRulings(state) must_== List(NextUpRuling(nextUpCommand, None))
    }
  DamageCommand should
    ask for resistance ${
      ApplyDamageCommand.requiredRulings(state) must_== List(AlterDamageRuling("Resist: 5 fire", Reduce(5), None))
    }
  """
}
