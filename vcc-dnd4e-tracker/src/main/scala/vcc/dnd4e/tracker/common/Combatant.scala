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
package vcc.dnd4e.tracker.common

/**
 * Combatant combat representations, including tracking of health, effect and comments. Initiative is managed by
 * InitiativeOrder.
 * @param definition Defining characteristics of the combatant
 * @param comment User defined comments for this combatant
 * @param health HealthTrack for this combatant
 * @param effects Effects that are currently in place.
 */
case class Combatant(definition: CombatantRosterDefinition, comment: String, health: HealthTracker, effects: EffectList) {

  /**
   * Replace current HealthDefinition, but preserve damage already suffered.
   * @param newDef New character definitions
   * @return A new copy of the combatant with replaced definition and altered HealthTracker
   */
  def updateDefinition(newDef: CombatantRosterDefinition): Combatant = {
    this.copy(
      definition = newDef,
      health = this.health.replaceHealthDefinition(newDef.entity.healthDef)
    )
  }

  /**
   * Apply rest to relevant fields in combatant, this include health and effects. The resulting combatant will have no
   * effect that expire at the end of the combat (Stance, EoE) and health updated accordingly.
   * @param duration Indicate if the rest is extended or not
   * @return A new combatant updated according to rest rules.
   */
  def applyRest(duration: RestDuration.Value): Combatant = {
    this.copy(
      health = health.rest(duration),
      effects = effects.transformAndFilter(EffectTransformation.applyRest)
    )
  }

  /**
   * Alias to the name in the definition
   */
  def name = definition.entity.name

  /**
   * Get combatant type
   */
  def combatantType = definition.entity.combatantType
}

/**
 *  Defines the major CombatantRoster data for a tracker.transactional.Combatant, it is also used for views.
 */
case class CombatantRosterDefinition(cid: CombatantID, alias: String, entity: CombatantEntity)

object Combatant {
  def apply(definition: CombatantRosterDefinition): Combatant = {
    Combatant(
      definition = definition,
      health = HealthTracker.createTracker(definition.entity.healthDef),
      effects = EffectList(definition.cid, Nil),
      comment = ""
    )
  }

  case object RosterFactory extends RosterCombatantFactory[Combatant] {

    def replaceDefinition(combatant: Combatant, newDefinition: CombatantRosterDefinition): Combatant = {
      combatant.updateDefinition(newDefinition)
    }

    def createCombatant(definition: CombatantRosterDefinition): Combatant = apply(definition)
  }
}