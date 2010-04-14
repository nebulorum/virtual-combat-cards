/**
 *  Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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
//$Id4
package vcc.dnd4e.domain.tracker.transactional

import vcc.dnd4e.domain.tracker.common._

/**
 * CombatState provides an aggregate of the CombatantRoster, InitiativeOrder, and CombatMetaData. It also provides
 * some helper extractors for the tracker logic.
 */
class CombatState(val order: InitiativeOrder, val roster: CombatantRoster, val metaData: CombatMetaData) {

  /**
   *  Default constructor that creates all the necessary objects.
   * @param initialText Text to be placed in CombatMetaData text.
   */
  def this(initialText: String) = this (new InitiativeOrder(), new CombatantRoster(), new CombatMetaData(initialText))

  /**
   * This is an extractor to get the InitiativeTracker from a InitiativeOrderID
   */
  object initTrackerFromID {
    def unapply(orderID: InitiativeOrderID): Option[InitiativeTracker] =
      if (order.isDefinedAt(orderID)) Some(order.initiativeTrackerFor(orderID)) else None
  }

  /**
   * This is an extractor to get the Combatant from a CombatantID
   */
  object combatantFromID {
    def unapply(combID: CombatantID): Option[Combatant] =
      if (roster.isDefinedAt(combID))
        Some(roster.combatant(combID))
      else
        None
  }

  def inCombat: Boolean = metaData.inCombat
}