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
package vcc.dnd4e.web.util

import org.json4s.native.JsonMethods
import vcc.dnd4e.tracker.common._
import org.json4s._
import org.json4s.JsonDSL._

class StateFormatter {
  def format(state: CombatState): String = {
    val builder = new UnifiedSequenceTable.Builder
    builder.hideDead()
    formatState(builder.build(state).elements, state)
  }

  private def formatState(combatants: Seq[UnifiedCombatant], state: CombatState): String = {
    if (combatants.isEmpty)
      "[]"
    else
      "[\n" + formatCombatants(combatants, state).mkString(",\n") + "]"
  }

  private def formatCombatants(combatants: Seq[UnifiedCombatant], state: CombatState): Seq[String] =
    for (combatant <- combatants if combatant.isInOrder)
    yield formatCombatant(state, combatant)

  private def formatCombatant(state: CombatState, comb: UnifiedCombatant): String = {
    val ms: Map[String, JValue] = Seq(
      makeField("id", if (comb.isInOrder) comb.orderId.toLabelString else comb.combId.id),
      makeField("name", comb.name),
      makeField("status", comb.health.formattedStatus),
      makeOptionField("isCharacter", if (comb.isCharacter) Some(true) else None),
      makeOptionField("health", if (comb.isCharacter) Some(comb.health.formattedHitPoints) else None),
      makeOptionField("effects", makeEffects(state, comb.effects.effects))
    ).flatMap(x => x).toMap
    JsonMethods.compact(JsonMethods.render(ms.foldLeft(JObject())(_ ~ _)))
  }

  private def makeEffects(state: CombatState, effects: List[Effect]): Option[JValue] =
    seqToJsonArrayOption(
      for (effect <- effects if isPlayerVisible(state, effect))
      yield formatEffect(effect))

  private def seqToJsonArrayOption(list: List[JValue]):Option[JValue] = if (list.isEmpty) None else Some(list)

  private def isPlayerVisible(state: CombatState, effect: Effect) =
    isCharacter(state, effect.source) || isCharacter(state, effect.effectId.combId)

  private def isCharacter(state: CombatState, cid: CombatantID) =
    state.combatant(cid).combatantType == CombatantType.Character

  private def formatEffect(effect: Effect):JValue =
    ("description" -> effect.condition.description) ~ ("duration" -> effect.duration.shortDescription)

  private def makeField(key: String, value: JValue) = Some((key, value))

  private def makeOptionField(key: String, value: Option[JValue]) = value.map(value => (key, value))
}