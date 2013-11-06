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

import org.specs2.SpecificationWithJUnit
import org.specs2.matcher.DataTables
import org.specs2.specification.Example
import language.reflectiveCalls

class ConditionMatcherTest extends SpecificationWithJUnit with DataTables {

  import ConditionMatcher._

  def is =
    "splitFirstCondition" ^ e1 ^ endp ^
      "Regeneration matcher" ^ e2 ^ endp ^
      "Ongoing matcher" ^ e3 ^ endp ^
      "Resist matcher" ^ e4 ^ endp ^
      "Vulnerability matcher" ^ e5 ^ endp ^
      "Immunity matcher" ^ e6 ^ endp ^
      "Insubstantial matcher" ^ e7 ^ endp ^
      end

  private def e1 = {
    val obj = new ConditionMatcher[(String, Int)] {
      protected def findSubCondition(l: List[String]): Option[(String, Int)] = None
    }
    val cases = List(
      ("slowed and weak", List("slowed", "weak")),
      ("slowed aNd Weak", List("slowed", "Weak")),
      ("slowed and weak and     dead", List("slowed", "weak", "dead")),
      ("slowed", List("slowed")),
      ("slowed-> weak", List("slowed")),
      ("slowed -> weak", List("slowed"))
    )
    for ((str, ret) <- cases) yield {
      ("split " + str + " to " + ret) ! {
        obj.splitFirstCondition(str) must_== ret
      }
    }
  }

  private def e2 = {
    runCases(FirstRegenerate,
      ("regenerate 2", ("regenerate 2", 2)),
      ("regen 2", ("regen 2", 2)),
      ("regeneration 2", ("regeneration 2", 2)),
      ("Regeneration 2", ("Regeneration 2", 2)),
      ("+2 def -> regeneration 2", null),
      ("regeneration 2->+2 def", ("regeneration 2", 2)),
      ("regenerate 5 while bloodied", ("regenerate 5 while bloodied", 5)),
      ("regenerate 5 while bloodied and +2 defense", ("regenerate 5 while bloodied", 5)),
      ("+2 defense and regenerate 5 while bloodied", ("regenerate 5 while bloodied", 5)),
      ("+2 defense regenerate 5 while bloodied", ("regenerate 5 while bloodied", 5)),
      ("slowed and weakened", null),
      ("ongoing 4 fire", null)
    )
  }

  private def e3 = {
    runCases(FirstOngoing,
      ("ongoing 4 fire", ("ongoing 4 fire", 4)),
      ("ongoing 4 fire and immobilized", ("ongoing 4 fire", 4)),
      ("slowed and ongoing 4", ("ongoing 4", 4)),
      ("slowed ongoing 4", ("ongoing 4", 4)),
      ("ONgoing 10 (1d20) and spooked", ("ONgoing 10 (1d20)", 10)),
      ("regenerate 2", null),
      ("regen 2", null),
      ("ongoing 4 fire -> ongoing 10 cold", ("ongoing 4 fire", 4)),
      ("Ongooing 124 fire & thunder and insubstantial", null)
    )
  }

  private def e4 = {
    runCases(Resist,
      ("resist 4", ("Resist: 4", 4)),
      ("resist 14 all while bloodied ", ("Resist: 14 all while bloodied", 14)),
      ("Res 4", null),
      ("resist 10 fire and +2 def", ("Resist: 10 fire", 10)),
      ("+3 def and resist 12 cold", ("Resist: 12 cold", 12)),
      ("resist 10 fire", ("Resist: 10 fire", 10))
    )
  }

  private def e5 = {
    runCases(Vulnerability,
      ("vuln 4", ("Vulnerable: 4", 4)),
      ("slow and vuln 4", ("Vulnerable: 4", 4)),
      ("slow and vuln 14 all while in rage", ("Vulnerable: 14 all while in rage", 14)),
      ("slow and vul 4", null),
      ("Vulnera 6 fire", ("Vulnerable: 6 fire", 6))
    )
  }

  private def e6 = {
    runCases(Immunity,
      ("iMmune", "Immune:"),
      ("slow and Immune fire", "Immune: fire"),
      ("slow and vul 4", null),
      ("Immune poison nastiness ", "Immune: poison nastiness")
    )
  }

  private def e7 = {
    runCases(Insubstantial,
      ("Insub", "Insubstantial"),
      ("insubstantial when flying", "Insubstantial when flying"),
      ("slow and insubst", "Insubstantial"),
      ("slow and insubst blob", "Insubstantial blob"),
      ("slow and insu", null),
      ("Insubstantial poison nastiness ", "Insubstantial poison nastiness")
    )
  }

  type M[T] = {def unapply(x: String): Option[T]}

  private def runCases[R](matcher: M[R], cases: (String, R)*) = bla(cases, matcher)

  private def bla[R](cases: Seq[(String, R)], matcher: M[R]): Seq[Example] = {
    for ((str, ret) <- cases) yield {
      "match " + str ! {
        matcher.unapply(str) must_== Option(ret)
      }
    }
  }
}