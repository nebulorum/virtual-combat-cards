/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
//$Id$
package vcc.dnd4e.view.dnd

import vcc.dnd4e.view.UnifiedCombatantID
import java.awt.datatransfer.Transferable
import vcc.util.swing.dnd.GenTransferable

/**
 * Represents a action that must be applied on a UnifiedCombatant
 * @param description Text to add to drag area
 * @param action Partial that checks if the action can be applied to a specific UnifiedCombatantID
 */
case class UnifiedCombatantActionTransfer(description: String, action: PartialFunction[UnifiedCombatantID, Unit]) {

  import UnifiedCombatantActionTransfer._

  def toTransferable: Transferable = new GenTransferable(this, UnifiedCombatantDataFlavor, description)
}

object UnifiedCombatantActionTransfer {

  val UnifiedCombatantDataFlavor = GenTransferable.makeFlavor(classOf[UnifiedCombatantActionTransfer])

}