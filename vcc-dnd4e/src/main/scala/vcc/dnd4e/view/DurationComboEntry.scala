/*
 * Copyright (C) 2008-2014 - Thomas Santana <tms@exnebula.org>
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

import vcc.dnd4e.tracker.common.{Duration, UnifiedCombatantID}

/**
 * A combo box option that included the information to display and what to
 * generate as an output
 * @param text To appear on the ComboBox
 */
abstract class DurationComboEntry(text: String) {

  def isDefinedAt(source: UnifiedCombatantID, target: UnifiedCombatantID) : Boolean

  def generate(source: UnifiedCombatantID, target: UnifiedCombatantID): Duration

  override def toString: String = text
}

object DurationComboEntry {
  val durations = List(
    new BoundDurationComboEntry("End of source's next turn", Duration.Limit.EndOfNextTurn, true),
    new BoundDurationComboEntry("End of source's next turn, sustain", Duration.Limit.EndOfNextTurnSustain, true),
    new BoundDurationComboEntry("Start of source's next turn", Duration.Limit.StartOfNextTurn, true),
    new StaticDurationComboEntry("End of Encounter", Duration.EndOfEncounter),
    new StaticDurationComboEntry("Stance", Duration.Stance),
    new StaticDurationComboEntry("Rage", Duration.Rage),
    new StaticDurationComboEntry("Save End", Duration.SaveEnd),
    new StaticDurationComboEntry("Save End (Special)", Duration.SaveEndSpecial),
    new StaticDurationComboEntry("Other", Duration.Other),
    new BoundDurationComboEntry("End of target's next turn", Duration.Limit.EndOfNextTurn, false),
    new BoundDurationComboEntry("Start of target's next turn", Duration.Limit.StartOfNextTurn, false))
}

class StaticDurationComboEntry(text: String, duration: Duration) extends DurationComboEntry(text) {
  def isDefinedAt(source: UnifiedCombatantID, target: UnifiedCombatantID): Boolean = true

  def generate(source: UnifiedCombatantID, target: UnifiedCombatantID): Duration = duration
}

class BoundDurationComboEntry(text: String, limit: Duration.Limit.Value, ofSource: Boolean) extends DurationComboEntry(text) {
  def isDefinedAt(source: UnifiedCombatantID, target: UnifiedCombatantID): Boolean = if (ofSource) source.isInOrder else target.isInOrder

  def generate(source: UnifiedCombatantID, target: UnifiedCombatantID): Duration =
    Duration.RoundBound(if (ofSource) source.orderId else target.orderId, limit)
}