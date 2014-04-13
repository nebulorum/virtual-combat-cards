/*
 * Copyright (C) 2008-2014 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.application

import org.specs2.SpecificationWithJUnit
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.event._

class CombatSaveFileTest extends SpecificationWithJUnit with CombatStateBuilder {
  def is = "CombatSaveFile".title ^
    baseCases ^
    healthCases ^
    initiativeOrder ^
    effectHandling ^
    encodingSafe ^
    end

  def encodingSafe = {
    if (System.getProperty("file.encoding") != "UTF-8") {
      "test encoding issues" ^
        testCase("bad encoding", buildState(emptyState, SetCombatCommentEvent(Some("dragon’s")))) ^
        endp
    } else {
      endp
    }
  }

  def healthCases = {
    val cases = List(
      testCase("with damage", buildState(stateWithCombatant(), ApplyDamageEvent(comb1, 10))),
      testCase("with temp hp", buildState(stateWithCombatant(), SetTemporaryHitPointsEvent(combA, 7))),
      testCase("with dead", buildState(stateWithCombatant(), ApplyDamageEvent(combA, 100))),
      testCase("with reverted death", buildState(stateWithCombatant(), ApplyDamageEvent(combA, 100), RevertDeathEvent(combA)))
    )
    "heath cases" ^ cases ^ endp
  }

  def baseCases = {
    val cases = List(
      testCase("empty state", emptyState),
      testCase("combat comment", buildState(emptyState, SetCombatCommentEvent(Some("memorable")))),
      testCase("multiple combatants", stateWithCombatant()),
      testCase("combatants with comment",
        buildState(stateWithCombatant(), SetCombatantCommentEvent(combA, "a comment"))),
      testCase("base case", stateWithCombatant())
    )
    "base cases" ^ cases ^ endp
  }

  def initiativeOrder = {
    val order1A2 = buildState(stateWithCombatant(), addToOrder(combA, 3, 10), addToOrder(comb1, 5, 15), addToOrder(comb2, 6, 8))

    val cases = List(
      testCase("set initiative", buildState(stateWithCombatant(), addToOrder(combA, 3, 10))),
      testCase("set initiative complex", buildState(stateWithCombatant(), addToOrder(combA, 3, 10, 5), addToOrder(comb1, 5, 1))),
      testCase("all tied", buildState(stateWithCombatant(), addToOrder(combA, 3, 10, 10), addToOrder(comb1, 3, 10), addToOrder(comb2, 3, 10))),
      testCase("order 1, A, 2", order1A2),
      testCase("order 1, A, 2, start", buildState(order1A2, StartCombatEvent)),
      testCase("order 1, A, 2, start, reorder", buildState(order1A2, StartCombatEvent,
        MoveBeforeOtherEvent(InitiativeOrderID(comb2, 0), InitiativeOrderID(combA, 0))))
    )
    "initiative order cases" ^ cases ^ endp
  }

  private def effectHandling = {
    import vcc.dnd4e.tracker.common.Effect._

    def makeEventWithDuration(duration: Duration): AddEffectEvent = {
      AddEffectEvent(comb1, combA, Condition.Mark(comb1, permanent = true), duration)
    }

    def makeRoundBoundDuration(limit: Duration.Limit.Value): Duration = {
      Duration.RoundBound(InitiativeOrderID(CombatantID(limit.id.toString), limit.id), limit)
    }

    val event1 = AddEffectEvent(combA, comb1, Condition.Generic("effect", beneficial =  true), Duration.EndOfEncounter)
    val event2 = AddEffectEvent(combA, comb2, Condition.Generic("bad effect", beneficial = false), Duration.EndOfEncounter)
    val event3 = AddEffectEvent(combA, comb2, Condition.Mark(comb2, permanent = false), Duration.EndOfEncounter)
    val event4 = AddEffectEvent(comb1, combA, Condition.Mark(comb1, permanent = true), Duration.EndOfEncounter)
    val event5 = makeEventWithDuration(Duration.Stance)

    val durations = Duration.allStaticDurations ++
      Seq(
        Duration.RoundBound(InitiativeOrderID(combA, 1), Duration.Limit.EndOfNextTurn),
        Duration.RoundBound(InitiativeOrderID(comb2, 2), Duration.Limit.EndOfTurnSustain)) ++
      Duration.Limit.values.map(makeRoundBoundDuration)

    val cases = List(
      testCase("Generic beneficial", buildState(stateWithCombatant(), event1)),
      testCase("both generic events", buildState(stateWithCombatant(), event1, event2)),
      testCase("mark", buildState(stateWithCombatant(), event3)),
      testCase("both mark types", buildState(stateWithCombatant(), event3, event4)),
      testCase("stance duration", buildState(stateWithCombatant(), event5))
    )

    "effect type cases" ^
      cases ^
      durations.map(d => testCase(d + "duration", buildState(stateWithCombatant(), makeEventWithDuration(d)))) ^
      endp
  }

  private def testCase(testDescription: String, combatState: CombatState) = {
    val loadedCombatState = storeAndLoadToMemory(combatState)
    testDescription ! (loadedCombatState must_== combatState)
  }

  private def storeAndLoadToMemory(combatState: CombatState): CombatState = {
    loadFromByteArray(storeToByteArray(combatState))
  }

  private def stateWithCombatant(): CombatState = {
    buildRoster((Some(combA), "alias", entityFighter), (None, null, entityGoblin), (None, "boss", entityMinion))
  }

  private def storeToByteArray(combatState: CombatState): Array[Byte] = {
    val s = new CombatSaveFile()
    val os = new ByteArrayOutputStream()
    s.save(os, combatState)
    os.toByteArray
  }

  private def loadFromByteArray(inputBytes: Array[Byte]): CombatState = {
    val is = new ByteArrayInputStream(inputBytes)
    val loadedCombatState: CombatState = new CombatSaveFile().load(is)
    loadedCombatState
  }
}