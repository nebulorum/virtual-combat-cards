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
package vcc.dnd4e.web.services

import vcc.dnd4e.tracker.common.CombatState

trait StateViewService {
  def stateAfterChange(timeoutInMillis: Int): Option[CombatState]

  def currentState(): CombatState
}

object StateViewService {
  private var instance: StateViewService = null

  def getInstance: StateViewService = instance

  def setInstance(instance: StateViewService) {
    this.instance = instance
  }
}