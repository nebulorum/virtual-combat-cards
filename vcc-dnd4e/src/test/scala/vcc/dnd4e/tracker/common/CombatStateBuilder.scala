/*
 * Copyright (C) 2014-2014 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.tracker.common

import vcc.infra.datastore.naming.EntityID
import vcc.dnd4e.tracker.event.{AddCombatantToOrderEvent, AddCombatantEvent}
import vcc.tracker.Event

trait CombatStateBuilder {
  protected val combA = CombatantID("A")
  protected val comb1 = CombatantID("1")
  protected val comb2 = CombatantID("2")
  protected val emptyState = CombatState.empty

  protected val entityFighter = createCombatantEntity("Fighter", CharacterHealthDefinition(30), 5)
  protected val entityGoblin = createCombatantEntity("Goblin", MonsterHealthDefinition(25), 3)
  protected val entityMinion = createCombatantEntity("Goblin-mini", MinionHealthDefinition, 1)

  protected def buildRoster(entities: (Option[CombatantID], String, CombatantEntity)*) =
    emptyState.transitionWith(entities.map(t => AddCombatantEvent(t._1, t._2, t._3)).toList)

  def addToOrder(comb: CombatantID, bonus: Int, rolls: Int*) =
    AddCombatantToOrderEvent(InitiativeDefinition(comb, bonus, rolls.toList))

  protected def createCombatantEntity(name: String, healthDefinition: HealthDefinition, initiativeBonus: Int) = {
    CombatantEntity(
      EntityID.generateRandom().asStorageString,
      name, healthDefinition, initiativeBonus,
      "<html><body>" + name + "</body></html>")
  }

  protected def buildState(baseState: CombatState, events: Event[CombatState]*): CombatState = {
    baseState.transitionWith(events.toList)
  }

}