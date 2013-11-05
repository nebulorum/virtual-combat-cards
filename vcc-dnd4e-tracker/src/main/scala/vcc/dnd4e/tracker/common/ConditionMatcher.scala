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

import annotation.tailrec


abstract class ConditionMatcher {

  protected def findSubCondition(l: List[String]): Option[(String, Int)]

  /**
   * Returns the first Ongoing sub condition in the condition text.
   */
  def unapply(text: String): Option[(String, Int)] = {
    findSubCondition(splitFirstCondition(text))
  }

  /**
   * Split condition on the <b>and</b> word.
   */
  def splitFirstCondition(text: String): List[String] = {
    ConditionMatcher.splitProgression(text)(0).split( """(?i)\s*(?:and)\s*""").toList
  }

}

/**
 * Helper functions and extractors to analyse condition text.
 */
object ConditionMatcher {

  /**
   * Returns the first Regenerate condition in the condition text.
   */
  object FirstRegenerate extends ConditionMatcher {
    private val regenRE = """(?i).*(regen\w*\s+(\d+).*)""".r

    @tailrec
    protected def findSubCondition(l: List[String]): Option[(String, Int)] = {
      l match {
        case Nil => None
        case this.regenRE(full, hint) :: rest => Some((full, hint.toInt))
        case head :: rest => findSubCondition(rest)
      }
    }
  }

  /**
   * Returns the first Ongoing sub condition in the condition text.
   */
  object FirstOngoing extends ConditionMatcher {
    private val ongoingRE = """(?i).*(ongoing\s+(\d+).*)""".r

    @tailrec
    protected def findSubCondition(l: List[String]): Option[(String, Int)] = {
      l match {
        case Nil => None
        case this.ongoingRE(full, hint) :: rest => Some((full, hint.toInt))
        case head :: rest => findSubCondition(rest)
      }
    }
  }

  /**
   * Returns the resist condition in the condition text.
   */
  object Resist extends ConditionMatcher {
    private val resistMatcher = """(?i).*resist.*\s+(\d+)\s*(.*)""".r

    @tailrec
    protected def findSubCondition(l: List[String]): Option[(String, Int)] = {
      l match {
        case Nil => None
        case this.resistMatcher(hint, qualifier) :: rest => Some((s"Resist: $hint ${qualifier.trim}".trim, hint.toInt))
        case head :: rest => findSubCondition(rest)
      }
    }
  }

  /**
   * Split Condition by the progression indicators (-> or / ).
   */
  def splitProgression(text: String) = text.split( """\s*(?:\->|\/)\s*""")
}