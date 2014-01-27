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
package vcc.dnd4e.view.helper

import org.specs2.SpecificationWithJUnit
import org.specs2.matcher.DataTables

class DamageParserTest extends SpecificationWithJUnit with DataTables {

  import DamageParser._

  def is = "base parsers" ! baseMatcher

  def baseMatcher = {
    "term" | "parsed" |
      "2 + 3" !! opAdd(2, 3) |
      "2 * S + 4" !! opAdd(Op("*", 2, "s"), 4) |
      "2 * (b + 1) + 4" !! opAdd(Op("*", 2, opAdd("b", 1)), 4) |
      "1 + 2 + 3 + S" !! opAdd(opAdd(opAdd(1, 2), 3), "s") |
      "41" !! NumberTerm(41) |> {
      (input: String, term: Term) => DamageParser.parseString(input) must_== term
    }
  }

  implicit private def i(v:Int):Term = NumberTerm(v)
  implicit private def s(s:String):Term = SymbolTerm(s.toLowerCase)
  private def opAdd(l:Term, r:Term) = Op("+",l , r)
}