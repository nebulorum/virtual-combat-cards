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
package vcc.dnd4e.tracker.common

import org.specs2.SpecificationWithJUnit

class EffectTest extends SpecificationWithJUnit {

  def is =
    "Duration tests" ^
      toAndFromString ^ endp

  end

  def toAndFromString = {
    for (duration <- Duration.allStaticDurations) yield {
      "to string " + duration ! (Duration.staticDurationFromDescription(duration.shortDescription).get must_== duration)
    }
  }
}