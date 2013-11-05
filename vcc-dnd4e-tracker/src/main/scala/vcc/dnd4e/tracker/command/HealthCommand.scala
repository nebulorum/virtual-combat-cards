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

import vcc.dnd4e.tracker.common.{CombatState, CombatantID}
import vcc.dnd4e.tracker.event._
import vcc.tracker.{Ruling, IllegalActionException, Event}
import vcc.dnd4e.tracker.ruling.AlterDamageRuling
import vcc.dnd4e.tracker.event.AlterDamageIndicationEvent.Reduce
import vcc.dnd4e.tracker.common.ConditionMatcher.Resist

/**
 * Base health modification transition.
 */
abstract class HealthCommand(target: CombatantID) extends CombatStateCommand {
  /**
   * Override this one
   */

  protected def makeTransitionEvent(): CombatStateEvent

  def generateEvents(state: CombatState): List[Event[CombatState]] = {
    if (!state.roster.isDefinedAt(target))
      throw new IllegalActionException("Combatant " + target + " not in combat")

    makeTransitionEvent() :: Nil
  }
}

/**
 * Add to state that damaged should be applied
 * @param target
 * @param amount
 */
case class AddDamageIndicationCommand(target: CombatantID, amount: Int) extends CombatStateCommand {
  def generateEvents(state: CombatState): List[Event[CombatState]] = {
    if (!state.roster.isDefinedAt(target))
      throw new IllegalActionException("Combatant " + target + " not in combat")

    List(SetDamageIndicationEvent(target, amount))
  }
}

case class AlterDamageIndicationCommand(change: AlterDamageIndicationEvent.Change) extends CombatStateCommand {
  def generateEvents(state: CombatState): List[Event[CombatState]] = List(AlterDamageIndicationEvent(change))
}

/**
 * Applies damage indication in state to a combatant and ends combat if all
 * combatants in the initiative order are dead.
 */
case object ApplyDamageCommand extends CombatStateCommand {

  def generateEvents(iState: CombatState): List[Event[CombatState]] = {
    if (iState.damageIndication.isEmpty)
      throw new IllegalStateException("No damage indication present")

    // We need to check if all combatant are after we apply damage
    val di = iState.damageIndication.get
    val damageEvent = ApplyDamageEvent(di.target, di.amount)
    val nState = damageEvent.transition(iState)

    if (nState.rules.areAllCombatantInOrderDead(nState)) {
      damageEvent :: ClearDamageIndicationEvent :: EndCombatEvent :: Nil
    } else {
      damageEvent :: ClearDamageIndicationEvent :: Nil
    }
  }

  override def requiredRulings(state: CombatState): List[Ruling[CombatState, _, _]] = {
    val who = state.damageIndication.get.target
    val targetEffects = state.combatant(who).effects
    val f : PartialFunction[String, Ruling[CombatState,_,_]] = {case Resist(text, hint) => AlterDamageRuling(text, Reduce(hint), None)}

    targetEffects.effects.map(_.condition.description).collect(f)
  }
}

/**
 * Heal a combatant .
 * @param target Target of healing
 * @param amount How many hit points to restore.
 */
case class HealCommand(target: CombatantID, amount: Int) extends HealthCommand(target) {
  protected def makeTransitionEvent(): CombatStateEvent = ApplyHealingEvent(target, amount)
}

/**
 * Set combatant temporary hit points.
 * @param target Target to get temporary hit points.
 * @param amount How many points to set.
 */
case class SetTemporaryHPCommand(target: CombatantID, amount: Int) extends HealthCommand(target) {
  protected def makeTransitionEvent(): CombatStateEvent = SetTemporaryHitPointsEvent(target, amount)
}

/**
 * Tick on of the character failed death saving throws.
 * @param target Target that failed check
 */
case class FailDeathSaveCommand(target: CombatantID) extends HealthCommand(target) {
  protected def makeTransitionEvent(): CombatStateEvent = FailDeathSaveEvent(target)
}

/**
 * Bring target back from dead (to 0 HP and dying).
 * @param target Target to restore
 */
case class RevertDeathCommand(target: CombatantID) extends HealthCommand(target) {
  protected def makeTransitionEvent(): CombatStateEvent = RevertDeathEvent(target)
}
