/*
 * Copyright (C) 2013-2014 - Thomas Santana <tms@exnebula.org>
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

  def is =
    "base parsers" ! baseMatcher ^
      "damage generator parsers" ! damageParser ^
      "some error case" ! invalidInputs ^
      "symbolic expression applier" ! applySymbolicExpression ^
      "damage applier" ! applyDamageExpression ^
      "dice roller" ! testDice ^
      end


  private def baseMatcher = {
    "term" | "parsed" |
      "2 + 3" !! opAdd(2, 3) |
      "2 - 4" !! opSub(2, 4) |
      "2 * S + 4" !! opAdd(Op("*", 2, "s"), 4) |
      "2 * (b + 1) + 4" !! opAdd(Op("*", 2, opAdd("b", 1)), 4) |
      "1 + 2 + 3 + S" !! opAdd(opAdd(opAdd(1, 2), 3), "s") |
      "41" !! NumberTerm(41) |> {
      (input: String, term: Term) => DamageParser.parseSymbolicExpression(input) must_== Right(term)
    }
  }

  private def damageParser = {
    "expression" | "parsed" |
      "2 + 3" !! opAdd(2, 3) |
      "2d6" !! d(2, 6) |
      "d6" !! d(1, 6) |
      "d6 - 2" !! opSub(d(1, 6), 2) |
      "2d10+7" !! opAdd(d(2, 10), 7) |
      "d6 + 2d4 + 5" !! opAdd(opAdd(d(1, 6), d(2, 4)), 5) |
      "(1d6 + 1) * 3" !! Op("*", opAdd(d(1, 6), 1), 3) |
      "1d8" !! d(1, 8) |
      "43" !! NumberTerm(43) |> {
      (input: String, term: Term) => DamageParser.parseDamageExpression(input) must_== Right(term)
    }
  }

  private def invalidInputs = {
    "expression" | "isDamage" |
      "2 + a" !! true |
      "2 + s" !! true |
      "2 + 1d4" !! false |
      "2 + a" !! false |> {
      (input, isDamage) =>
        val result = if (isDamage) DamageParser.parseDamageExpression(input) else DamageParser.parseSymbolicExpression(input)
        result must beLeft
    }
  }

  private def applySymbolicExpression = {
    "expression" | "values" | "result" |
      "3 + 2 * S" !! Map("s" -> 10) !! 23 |
      "3 + 2 * S + B" !! Map("s" -> 10, "b" -> 21) !! 44 |
      "3 + 2 " !! Map.empty[String, Int] !! 5 |> {
      (input: String, definitions: Map[String, Int], result: Int) =>
        DamageParser.parseSymbolicExpression(input).right.get.apply(definitions) must_== result
    }
  }

  private def applyDamageExpression = {
    "expression" | "values" | "result" |
      "3 + 3d6" !! Map("max" -> 1) !! 21 |
      "3 + 7d1 " !! Map.empty[String, Int] !! 10 |
      "3 + 2 " !! Map.empty[String, Int] !! 5 |> {
      (input: String, definitions: Map[String, Int], result: Int) =>
        DamageParser.parseDamageExpression(input).right.get.apply(definitions) must_== result
    }
  }

  private def testDice = {
      "expression" | "range" |
      "1 + d8 " !! (2 to 9) |
      "1 + d4 + d6 + d8 " !! (4 to 19) |
      "2d6" !! (2 to 12) |> {
        (expression, range) =>
           val term = DamageParser.parseDamageExpression(expression).right.get
          (1 to 100).map(_ => term(Map())).forall(range contains) must beTrue
      }
  }

  implicit private def i(v: Int): Term = NumberTerm(v)

  implicit private def s(s: String): Term = SymbolTerm(s.toLowerCase)

  implicit private def d(n: Int, s: Int): Term = DiceTerm(n, s)

  private def opAdd(l: Term, r: Term) = Op("+", l, r)

  private def opSub(l: Term, r: Term) = Op("-", l, r)
}