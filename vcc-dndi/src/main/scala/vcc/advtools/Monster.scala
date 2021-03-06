/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.advtools

import vcc.dndi.reader.Parser.IconType
import vcc.dndi.reader.Usage
import vcc.dndi.common.FormattedText.Block

object Monster {

  case class BestiaryTaxonomy(size: String, origin: String, creatureType: String, keyword: Option[String], race: Option[String]) {
    def singleDescription = (size + " " + origin + " " + creatureType +
      keyword.map(" (%s)".format(_)).getOrElse("") + race.map(", %s".format(_)).getOrElse("")).toLowerCase.capitalize
  }

  case class GroupTaxonomy(role: String, groupRole: String, isLeader: Boolean, level: Int, experience: Int) {
    def completeRole = normalizeRole(groupRole) + " " + role + (if (isLeader) " (Leader)" else "")

    private def normalizeRole(role: String) = role match {
      case "Standard" => ""
      case s => s
    }
  }

  case class Defense(ac: Int, fortitude: Int, reflex: Int, will: Int)

  case class AbilityScores(strength: Int, dexterity: Int, constitution: Int, intelligence: Int, wisdom: Int, charisma: Int)

  case class BaseStats(hitPoint: Int, initiative: Int, actionPoints: Int, saveBonus: Int)

  trait Susceptibility {
    def damageType: String

    def amount: Int
  }

  case class Resistance(damageType: String, amount: Int) extends Susceptibility

  case class Vulnerability(damageType: String, amount: Int) extends Susceptibility

  case class Immune(damageType: String) extends Susceptibility {
    def amount = Integer.MAX_VALUE
  }

  sealed trait AttackType {
    def asIcons(): Seq[IconType.Value]
  }

  object AttackType {
    private val iconMapping = Map[String, IconType.Value](
      "melee" -> IconType.Melee,
      "close blast" -> IconType.Close,
      "close burst" -> IconType.Close,
      "ranged" -> IconType.Range,
      "area burst" -> IconType.Area,
      "area" -> IconType.Area)

    def asNormalIcons(attackType: String): Seq[IconType.Value] =
      attackType.split("\n").flatMap(n => iconMapping.get(n.toLowerCase))

    def asBasicIcons(attackType: String) = asNormalIcons(attackType).map(IconType.iconAsBasic)
  }

  case class BasicAttack(attackType: String) extends AttackType {
    def asIcons(): Seq[IconType.Value] = AttackType.asBasicIcons(attackType)
  }

  case class NormalAttack(attackType: String) extends AttackType {
    def asIcons(): Seq[IconType.Value] = AttackType.asNormalIcons(attackType)
  }

  case object NonAttack extends AttackType {
    def asIcons(): Seq[IconType.Value] = Nil
  }

  case class Power(powerName: String,
                   action: String, usage: Usage, attackType: AttackType,
                   keywords: Set[String], text: Block)

  trait BaseCreatureTrait

  case class Aura(name: String, radius: Int, keywords: Option[String], details: String) extends BaseCreatureTrait

  case class CreatureTrait(name: String, keywords: Option[String], details: String) extends BaseCreatureTrait

}