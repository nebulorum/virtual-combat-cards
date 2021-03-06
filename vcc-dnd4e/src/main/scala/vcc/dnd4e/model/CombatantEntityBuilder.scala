/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.model

import vcc.dnd4e.compendium.{CombatantEntity => CompendiumCombatantEntity, MonsterEntity, CharacterEntity}
import vcc.dnd4e.tracker.common._

object CombatantEntityBuilder {

  /**
   * Build a valid combatant form a CompendiumEntity
   */
  def fromCompendiumCombatantEntity(comp: CompendiumCombatantEntity): CombatantEntity = {
    val healthDef: HealthDefinition = comp match {
      case monster: MonsterEntity if(monster.hp.value == 1) => MinionHealthDefinition
      case monster: MonsterEntity => MonsterHealthDefinition(monster.hp.value)
      case character: CharacterEntity => CharacterHealthDefinition(comp.hp.value)
      case s => throw new Exception("Unexpected Entity type: " + s)
    }

    CombatantEntity(comp.eid.asStorageString, comp.name.value, healthDef,
      comp.initiative.value, comp.generateStatBlock())
  }
}