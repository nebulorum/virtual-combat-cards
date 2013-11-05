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
package vcc.dnd4e.tracker.event

import vcc.dnd4e.tracker.common.{DamageIndication, HealthTracker, CombatState, CombatantID}

abstract class HealthEvent extends CombatStateEvent {
  val target: CombatantID

  protected def transitionHealth(ht: HealthTracker): HealthTracker

  def transition(iState: CombatState): CombatState = iState.lensFactory.combatantHealth(target).modIfChanged(iState, ht => transitionHealth(ht))
}

case class SetDamageIndicationEvent(target: CombatantID, amount: Int) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState =
    iState.copy(damageIndication = Some(DamageIndication(target, amount)))
}

case object ClearDamageIndicationEvent extends CombatStateEvent {
  def transition(iState: CombatState): CombatState =
    iState.copy(damageIndication = None)
}

object AlterDamageIndicationEvent {
  trait Change {
    def modify(x: Int): Int
  }

  case class Reduce(amount:Int) extends Change {
    def modify(x: Int): Int = Math.max(0, x - amount)
  }

  case object Half extends Change {
    def modify(x: Int): Int = x / 2
  }

  case class Increase(amount: Int) extends Change {
    def modify(x: Int): Int = x + amount
  }
}

case class AlterDamageIndicationEvent(change: AlterDamageIndicationEvent.Change) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState =
    iState.copy(damageIndication = iState.damageIndication.map(di => di.copy(amount = change.modify(di.amount))))
}

case class ApplyDamageEvent(target: CombatantID, amount: Int) extends HealthEvent {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.applyDamage(amount)
}

case class ApplyHealingEvent(target: CombatantID, amount: Int) extends HealthEvent {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.heal(amount)
}

case class SetTemporaryHitPointsEvent(target: CombatantID, amount: Int) extends HealthEvent {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.setTemporaryHitPoints(amount)
}

case class FailDeathSaveEvent(target: CombatantID) extends HealthEvent {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.failDeathSave()
}

case class RevertDeathEvent(target: CombatantID) extends HealthEvent {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.raiseFromDead()
}