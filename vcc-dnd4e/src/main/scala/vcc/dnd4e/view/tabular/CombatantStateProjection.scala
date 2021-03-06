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
package vcc.dnd4e.view.tabular

import vcc.dnd4e.tracker.common.UnifiedCombatant
import vcc.util.swing.TableModelRowProjection

object CombatantStateProjection extends TableModelRowProjection[UnifiedCombatant] {
  override val columns = List[(String, java.lang.Class[_])](
    ("ID", classOf[java.lang.String]),
    ("Name", classOf[String]),
    ("Health", classOf[String]),
    ("Status", classOf[String]),
    ("T#/R", classOf[String]),
    ("Sequence", classOf[String])
  )

  def apply(col: Int, comb: UnifiedCombatant): java.lang.Object = {
    col match {
      case 0 => if (comb.isInOrder) comb.orderId.toLabelString else comb.combId.id
      case 1 => (if (comb.alias != null) "[" + comb.alias + "] " else "") + comb.name
      case 2 => comb.health.formattedHitPoints
      case 3 => comb.health.formattedStatus
      case 4 => if (comb.isInOrder) (comb.initiative.round + " / " + comb.initiative.initScore) else "-"
      case 5 => if (comb.isInOrder) comb.initiative.state else "-"
    }
  }

  val setter = null
}