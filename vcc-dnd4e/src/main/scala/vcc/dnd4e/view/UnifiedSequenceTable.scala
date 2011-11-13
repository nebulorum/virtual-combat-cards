/*
 *
 *  Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.view

import helper.{ReserveViewBuilder, InitiativeOrderViewBuilder}
import vcc.dnd4e.tracker.common.{CombatantID, InitiativeOrderID}
import vcc.dnd4e.domain.tracker.common.CombatStateView

/**
 * Provides a single linear and random access table with all combatants.
 * @param elements The list of UnifiedCombatant in the order
 * @param state The state that was used to build the sequence
 */
class UnifiedSequenceTable(val elements: Array[UnifiedCombatant], val state: CombatStateView) {

  /**
   * Get UnifiedCombatant at position idx
   * @param idx Index of the UnifiedCombatant on the table
   */
  def apply(idx: Int): UnifiedCombatant = elements(idx)

  /**
   * Get a combatant based on its orderId and combId. This is the proper way to get combatants that are not in order.
   */
  def apply(combId: CombatantID, orderId: InitiativeOrderID): UnifiedCombatant = {
    val m = elements.find(e => e.combId == combId && orderId == e.orderId)
    if (m.isDefined) m.get else null
  }

  /**
   * Get a combatant with InitiativeOrderID (will infer CombatantID from the order ID)
   */
  def apply(orderId: InitiativeOrderID): UnifiedCombatant = {
    if (orderId != null) {
      val m = elements.find(e => e.matches(orderId))
      if (m.isDefined) m.get else null
    } else null
  }

  /**
   * Get a combatant option, based on an option on UnifiedCombatantID.
   */
  def combatantOption(uid: Option[UnifiedCombatantID]): Option[UnifiedCombatant] =
    if (uid.isDefined) elements.find(e => e.matches(uid))
    else None

  /**
   * Return the first UnifiedCombatant in the robin.
   */
  def orderFirst(): Option[UnifiedCombatant] = state.nextUp.map(orderId => this(orderId.combId, orderId))

  /**
   * Return the UnifiedCombatantID of the first combatant in the robin.
   */
  def orderFirstId(): Option[UnifiedCombatantID] = state.nextUp.map(orderId => UnifiedCombatantID(orderId.combId, orderId))
}

/**
 * Companion object for UnifiedSequenceTable with is used to build the sequences
 */
object UnifiedSequenceTable {

  /**
   * This service object will used a ReserveViewBuilder and a InitiativeOrderViewBuilder to build a unified array with all
   * the combatant in a single Array.
   */
  def buildList(combatState: CombatStateView, orderBuilder: InitiativeOrderViewBuilder, reserveBuilder: ReserveViewBuilder): UnifiedSequenceTable = {
    val order = orderBuilder.buildOrder(combatState).map(e => new UnifiedCombatant(e.combId, combatState.initiativeTrackerFromID(e), combatState.combatantViewFromID(e.combId)))
    val reserve = reserveBuilder.buildReserve(combatState).map(e => new UnifiedCombatant(e, null, combatState.combatantViewFromID(e)))
    new UnifiedSequenceTable((order ++ reserve).toArray, combatState)
  }
}