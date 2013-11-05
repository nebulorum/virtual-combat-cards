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

import vcc.tracker.{Command, Ruling}
import vcc.dnd4e.tracker.common.CombatState
import vcc.dnd4e.tracker.event.AlterDamageIndicationEvent
import vcc.dnd4e.tracker.command.AlterDamageIndicationCommand

case class AlterDamageRuling(question: String, change: AlterDamageIndicationEvent.Change, decision: Option[Boolean])
  extends Ruling[CombatState, Boolean, AlterDamageRuling] {
  /**
   * Check to see if ruling refer to the same subject. This MUST consider the class and the argument
   * to avoid mixing subjects form different classes with similar arguments.
   */
  def isRulingSameSubject(otherRuling: Ruling[CombatState, _, _]): Boolean =
    otherRuling match {
      case AlterDamageRuling(q, c, _) => question == q && change == c
      case _ => false
    }

  def userPrompt(state: CombatState): String = question

  /**
   * This is the method must be overridden to generate appropriate events for a given decision. If this method is called
   * the ruling has a valid decision.
   * @param state Current state
   */
  protected def commandsFromDecision(state: CombatState): List[Command[CombatState]] =
    if(decision.isDefined && decision.get)
      List(AlterDamageIndicationCommand(change))
    else
      Nil

  /**
   * Create a new ruling with a decision specified. This new ruling can be used to generate commands from the decision.
   * @param decision A decision of the appropriate type
   * @return A new ruling of type R with the a valida ruling.
   */
  def withDecision(decision: Boolean): AlterDamageRuling = this.copy(decision = Some(decision))
}