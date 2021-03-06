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
package vcc.tracker

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import org.specs2.mock.Mockito
import helper._
import collection.immutable.List
import java.lang.String
import vcc.tracker.ActionDispatcher.{IllegalCommandException, IllegalEventException, RulingsAndDecisionsMismatchException}

class ActionDispatcherTest extends SpecificationWithJUnit with Mockito {

  private val actionWithSingleCommandAndEvent = FlexAction(FlexCommand(10))

  private val rulingWhere = FlexRuling("where", None)
  private val rulingWhat = FlexRuling("what", None)
  private val decisionWhat = makeDecision("what")
  private val decisionSorry = makeDecision("sorry")
  private val commandWithWhatWhereRuling = new FlexCommand(List("what", "where"), Nil)
  private val commandWithWhatRuling = new FlexCommand(List("what"), Nil)

  trait context extends Scope {
    val startState = State(0)
    val dispatcher = ActionDispatcher.getDispatcher(startState)

    val mockRulingProvider = mock[RulingProvider[State]]
    mockRulingProvider.provideRulingFor(any) returns Nil
    dispatcher.setRulingProvider(mockRulingProvider);

    protected def mockFlexRulingDecision(state: State, command: Command[State], decisions: flexDecision*) {
      val expectedRulings: List[FlexRuling] = decisions.map(d => FlexRuling(d.prompt, None)).toList
      val providedDecisions = decisions.map(d => FlexRuling(d.prompt, Some(d.decisionCommands.toList))).toList

      mockRulingProvider.provideRulingFor(RulingContext(state, command, expectedRulings)) returns providedDecisions
    }

    protected case class flexDecision(prompt: String, decisionCommands: Command[State]*)

  }

  "create handler" in new context {
    dispatcher must not beNull;
  }

  "create new dispatcher from factory method" in {
    val ad = ActionDispatcher.getDispatcher(State(10))
    ad must not beNull;
  }

  "dispatcher should only handle a single dispatch" in new context {
    dispatcher.handle(actionWithSingleCommandAndEvent)
    dispatcher.handle(actionWithSingleCommandAndEvent) must throwA[IllegalStateException]
  }

  "accept a dispatch of a command" in new context {
    dispatcher.handle(actionWithSingleCommandAndEvent)
    dispatcher.resultState.get must_== State(10)
  }

  "dispatch loop action that generates multiple commands" in new context {
    dispatcher.handle(LoopToAction(10, 3))
    dispatcher.resultState.get must_== State(12)
  }

  "detect infinite loop in dispath and commandSteam loop" in new context {
    dispatcher.handle(LoopToAction(10, 0)) must throwA[InfiniteLoopException]
  }

  "not detect infinite loop on action that does not change state but changes stream" in new context {
    dispatcher.handle(FlexAction(FlexCommand(0)))
    dispatcher.resultState must_== Some(startState)
  }

  "handle action with single Command and Single Event" in new context {
    dispatcher.handle(FlexAction(FlexCommand(1)))
    dispatcher.resultState.get must_== State(1)
  }

  "handle action with single Command and multiple Event" in new context {
    dispatcher.handle(FlexAction(FlexCommand(1, 2, 3)))
    dispatcher.resultState.get must_== State(6)
  }

  "handle action with multiple Command each with single Event" in new context {
    dispatcher.handle(FlexAction(FlexCommand(2), FlexCommand(3)))
    dispatcher.resultState.get must_== State(5)
  }

  "handle action with multiple Command each with multiple Event" in new context {
    dispatcher.handle(FlexAction(FlexCommand(1, 2, 3), FlexCommand(4, 5)))
    dispatcher.resultState.get must_== State(15)
  }

  "handle Action with single Command single Ruling that results in one Command" in new context {
    mockFlexRulingDecision(startState, FlexCommand("what"), flexDecision("what", FlexCommand(14)))

    dispatcher.handle(FlexAction(FlexCommand("what")))

    dispatcher.resultState.get must_== State(14)
    there was one(mockRulingProvider).provideRulingFor(RulingContext(startState, FlexCommand("what"), List(rulingWhat)))
  }

  "handle Action with single Command single Ruling that results in one Command follow by original events" in new context {
    val command = FlexCommand("what", 1, 1)
    mockFlexRulingDecision(startState, command, flexDecision("what", FlexCommand(SetStateEvent(5))))

    dispatcher.handle(FlexAction(command))

    dispatcher.resultState.get must_== State(7)
    there was one(mockRulingProvider).provideRulingFor(RulingContext(startState, command, List(rulingWhat)))
  }

  "handle Action with single Command that produces several Ruling with multiple commands of multiple events" in new context {
    val command = new FlexCommand(List("what", "where"), List(7, 8))
    mockFlexRulingDecision(startState, command,
      flexDecision("what", FlexCommand(1), FlexCommand(2, 3)),
      flexDecision("where", FlexCommand(4), FlexCommand(5, 6)))

    dispatcher.handle(FlexAction(command))

    dispatcher.resultState.get must_== State(36)
  }

  "handle Action with two Commands that produces single Ruling with multiple commands of multiple events" in new context {
    mockFlexRulingDecision(startState, FlexCommand("what"), flexDecision("what", FlexCommand(1, 2)))
    mockFlexRulingDecision(State(3), FlexCommand("where", 5), flexDecision("where", FlexCommand(3, 4)))

    dispatcher.handle(FlexAction(FlexCommand("what"), FlexCommand("where", 5)))

    dispatcher.resultState.get must_== State(15)
  }

  "handle Action with single Ruling with requires rulings" in new context {
    mockFlexRulingDecision(startState, FlexCommand("what"), flexDecision("what", FlexCommand(1), FlexCommand("where")))
    mockFlexRulingDecision(State(1), FlexCommand("where"), flexDecision("where", FlexCommand(3, 4)))

    dispatcher.handle(FlexAction(FlexCommand("what")))

    dispatcher.resultState.get must_== State(8)
  }

  "handle Action with single Ruling with requires rulings which requires other ruling" in new context {
    mockFlexRulingDecision(startState, FlexCommand("what"), flexDecision("what", FlexCommand(1), FlexCommand("where")))
    mockFlexRulingDecision(State(1), FlexCommand("where"), flexDecision("where", FlexCommand(3, 4), FlexCommand("where")))
    mockFlexRulingDecision(State(8), FlexCommand("where"), flexDecision("where", FlexCommand(4)))

    dispatcher.handle(FlexAction(FlexCommand("what")))

    dispatcher.resultState.get must_== State(12)
  }

  "throw exception if single Ruling is not answered" in new context {
    mockRulingProvider.provideRulingFor(
      RulingContext(startState, FlexCommand("what"), List(rulingWhat))) returns List(rulingWhat)

    dispatcher.handle(FlexAction(FlexCommand("what"))) must throwA[UndecidedRulingException]
  }

  "throw exception if not all Ruling have Decision" in new context {
    mockRulingProvider.provideRulingFor(
      RulingContext(startState, commandWithWhatWhereRuling, List(rulingWhat, rulingWhere))) returns List(decisionWhat)
    dispatcher.handle(FlexAction(commandWithWhatWhereRuling)) must
      throwA(new RulingsAndDecisionsMismatchException(List(rulingWhat, rulingWhere), List(decisionWhat)))
  }

  "throw exception if Decision does not match ruling" in new context {
    mockRulingProvider.provideRulingFor(
      RulingContext(startState, commandWithWhatWhereRuling, List(rulingWhat, rulingWhere))) returns
      List(makeDecision("what"), decisionSorry)

    dispatcher.handle(FlexAction(commandWithWhatWhereRuling)) must
      throwA(new RulingsAndDecisionsMismatchException(List(rulingWhat, rulingWhere), List(decisionWhat, decisionSorry)))
  }

  "throw exception if too many Decisions are provided" in new context {
    mockRulingProvider.provideRulingFor(
      RulingContext(startState, commandWithWhatRuling, List(rulingWhat))) returns List(decisionWhat, decisionSorry)

    dispatcher.handle(FlexAction(commandWithWhatRuling)) must
      throwA(new RulingsAndDecisionsMismatchException(List(rulingWhat), List(decisionWhat, decisionSorry)))
  }

  "throw exception and not provide state when Event transition fails" in new context {
    dispatcher.handle(FlexAction(FlexCommand(CrashEvent("no good")))) must
      throwA(new IllegalEventException(CrashEvent("no good"), null))
    dispatcher.resultState must_== None
  }

  "throw exception and not provide state when Commant fails to generate Events" in new context {
    dispatcher.handle(FlexAction(CrashCommand("no good"))) must
      throwA(new IllegalCommandException(CrashCommand("no good"), null))
    dispatcher.resultState must_== None
  }

  private def makeDecision(prompt: String, commands: Command[State]*): FlexRuling = {
    FlexRuling(prompt, Some(commands.toList))
  }

  private implicit def int2IncrementEvent(increment: Int): Event[State] = IncrementEvent(increment)
}