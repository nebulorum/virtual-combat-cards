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
import vcc.tracker.{IllegalActionException, Event}

class CombatStateCommandTest extends SpecificationWithJUnit with EventSourceSampleEvents with CombatStateEventSourceBehavior {

  def is =
    "Combat State Transactions" ^
      execAddCombatant ^
      defineCombatComment ^
      defineInitiativeBeforeCombatStart ^
      startCombat ^
      defineInitiativeAfterCombatStarted ^
      endCombat ^
      clearRoster ^
      combatantComments ^
      restCombatants ^
      end

  private def execAddCombatant = {
    val addA = AddCombatantCommand(Some(combA), null, entityPc1)
    "add combatant" ! (given(CombatState.empty) when addA andThen AddCombatantEvent(addA.cid, null, entityPc1))
  }

  private def defineCombatComment = {
    "Combat level comment" ^
      "Set comment to none if null" !
        (given(emptyState) when SetCombatCommentCommand(null) andThen SetCombatCommentEvent(None)) ^
      "Set comment" !
        (given(emptyState) when SetCombatCommentCommand("comment") andThen SetCombatCommentEvent(Some("comment"))) ^
      endp
  }

  private def combatantComments = {
    val cmd = SetCombatantCommentCommand(combA, "message")
    val exception = new IllegalActionException("Cant set comment: Combatant " + combA + " does not exist")
    "Combatant Comment" ^
      "Set comment on existent combatant" !
        (given(emptyState, evtAddCombA) when cmd andThen SetCombatantCommentEvent(combA, "message")) ^
      "Setting comment combatant on inexistent combatant should fail" !
        (given(emptyState) when cmd failWith exception) ^
      endp
  }

  private def defineInitiativeBeforeCombatStart = {
    val evtInitA = AddCombatantToOrderEvent(InitiativeDefinition(combA, 5, List(10)))
    val evtInitAOld = AddCombatantToOrderEvent(InitiativeDefinition(combA, 5, List(3)))
    val cmd = SetInitiativeCommand(InitiativeDefinition(combA, 5, List(10)))
    val exception = new IllegalActionException("Combatant " + combA + " not in combat roster")
    "Defining initiative before combat start" ^
      "define initiative" ! (given(emptyState, evtAddCombA) when cmd andThen evtInitA) ^
      "redefine initiative" ! (given(emptyState, evtAddCombA, evtInitAOld) when cmd andThen(RemoveCombatantFromOrderEvent(combA), evtInitA)) ^
      "cant set initiative if combatant not in roster" ! (given(emptyState) when cmd failWith exception) ^
      endp
  }

  private def startCombat = {
    val evtInitA = AddCombatantToOrderEvent(InitiativeDefinition(combA, 5, List(10)))
    val exceptionNotInOrder = new IllegalActionException("Must have at least on combatant in order")
    val exceptionAlreadyStart = new IllegalActionException("Combat already started")

    "StartCombat" ^
      "cannot start if no combatant present" ! (given(emptyState) when StartCombatCommand failWith exceptionNotInOrder) ^
      "cannot start if no combatant in order" ! (given(emptyState, evtAddCombA, evtAddCombNoId) when StartCombatCommand failWith exceptionNotInOrder) ^
      "start must work" ! (given(emptyState, evtAddCombA, evtAddCombNoId, evtInitA) when StartCombatCommand andThen StartCombatEvent) ^
      "start on started is not allowed" ! (given(emptyState, evtAddCombA, evtAddCombNoId, evtInitA, StartCombatEvent) when StartCombatCommand failWith exceptionAlreadyStart) ^
      endp
  }

  private def defineInitiativeAfterCombatStarted = {
    val evtInitA = AddCombatantToOrderEvent(InitiativeDefinition(combA, 5, List(10)))
    val evtInit1 = AddCombatantToOrderEvent(InitiativeDefinition(comb1, 5, List(10)))
    val evtInitAOld = AddCombatantToOrderEvent(InitiativeDefinition(combA, 5, List(3)))
    val cmd = SetInitiativeCommand(InitiativeDefinition(combA, 5, List(10)))
    val exception = new IllegalActionException("Combatant " + combA + " is already in order")
    "Defining initiative after combat start" ^
      "define initiative" ! (given(emptyState, evtAddCombA, evtAddCombNoId, evtInit1, StartCombatEvent) when cmd andThen evtInitA) ^
      "redefine initiative (illegal)" ! (given(emptyState, evtAddCombA, evtInitAOld, StartCombatEvent) when cmd failWith exception) ^
      endp
  }

  def endCombat = {
    val notStartedException = new IllegalActionException("Combat not started")
    "EndCombat" ^
      "cant end combat that was not started" !
        (given(emptyState) when EndCombatCommand failWith notStartedException) ^
      "cant end combat that was not started, even if it looks full" !
        (given(emptyState, evtAddCombA, evtInitA) when EndCombatCommand failWith notStartedException) ^
      "end a properly started combat" !
        (given(emptyState, evtAddCombA, evtInitA, StartCombatEvent) when EndCombatCommand andThen EndCombatEvent) ^
      endp
  }

  def clearRoster = {
    val exception = new IllegalActionException("Can not clear while in combat")
    val rA: Event[CombatState] = RemoveCombatantFromRosterEvent(combA)
    val r1: Event[CombatState] = RemoveCombatantFromRosterEvent(comb1)
    val r2: Event[CombatState] = RemoveCombatantFromRosterEvent(comb2)

    "Clear combat" ^
      "clear monster on started combat must fail" !
        (given(emptyState, evtAddCombA, evtInitA, StartCombatEvent) when ClearRosterCommand(true) failWith exception) ^
      "clear all on started combat must fail" !
        (given(emptyState, evtAddCombA, evtInitA, StartCombatEvent) when ClearRosterCommand(false) failWith exception) ^
      "remove all the combat" !
        (given(emptyState, evtAddCombA, evtAddCombNoId, evtAddComb2) when ClearRosterCommand(false) andThen contain(exactly(rA, r1, r2))) ^
      "remove only monsters" !
        (given(emptyState, evtAddCombA, evtAddCombNoId, evtAddComb2) when ClearRosterCommand(true) andThen contain(exactly(r1, r2))) ^
      endp
  }

  private def restCombatants = {
    val exception = new IllegalActionException("Can not rest during combat")
    val shortRest = RestCommand(RestDuration.ShortRest)
    val extendedRest = RestCommand(RestDuration.ExtendedRest)
    val sRest1: Event[CombatState] = RestCombatantEvent(comb1, RestDuration.ShortRest)
    val sRestA: Event[CombatState] = RestCombatantEvent(combA, RestDuration.ShortRest)
    val eRest1: Event[CombatState] = RestCombatantEvent(comb1, RestDuration.ExtendedRest)
    val eRestA: Event[CombatState] = RestCombatantEvent(combA, RestDuration.ExtendedRest)
    "Resting combatant" ^
      "rest should fail if combat is started" !
        (given(emptyState, evtAddCombA, evtInitA, StartCombatEvent) when shortRest failWith exception) ^
      "extend should fail if combat is started" !
        (given(emptyState, evtAddCombA, evtInitA, StartCombatEvent) when extendedRest failWith exception) ^
      "extend should rest all" !
        (given(emptyState, evtAddCombA, evtAddCombNoId) when extendedRest andThen contain(exactly(eRestA, eRest1))) ^
      "extend should rest all" !
        (given(emptyState, evtAddCombA, evtAddCombNoId) when shortRest andThen contain(exactly(sRestA, sRest1))) ^
      endp
  }
}