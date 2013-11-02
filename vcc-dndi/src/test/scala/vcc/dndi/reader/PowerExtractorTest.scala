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
package vcc.dndi.reader

import vcc.dndi.reader.Parser._
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.matcher.Matcher

class PowerExtractorTest extends SpecificationWithJUnit {

  "SomePowerDefinition" should {
    "read power with name and keyword" in {
      val parts = List(Icon(IconType.Melee), Key(" Name "), Text(" Keyword "), Icon(IconType.Separator), Key("Aura"), Text(" 1"))
      extract(parts) must_== Some(CompletePowerDefinition(Seq(IconType.Melee), "Name", "Keyword", AuraUsage("1")))
    }

    "read power with name and keyword but no usage separation, just bold" in {
      //<P class="flavor alt"> <B>Spider Burst</B> (Poison, Zone) <B></B></P>
      val parts = List(Text(" "), Key("Spider Burst"), Text(" (Poison, Zone) "), Key(""))
      extract(parts) must matchCompletePower(defining(Seq(), "Spider Burst", "(Poison, Zone)", NoUsage))
    }

    "read power with name, no keyword and no empty key as usage" in {
      //<P class="flavor alt"> <B>Mercurial Body</B> <B></B></P>)
      val parts = List(Text(" "), Key("Mercurial Body"), Text(" "), Key(""))
      extract(parts) must matchCompletePower(defining(Seq(), "Mercurial Body", null, NoUsage))
    }

    "read power with name, separator and round limited At-Will" in {
      //<P class="flavor alt"> <B>Mercurial Body</B> <B></B></P>)
      val parts = List(Text(" "), Key("Fey Light"), Text(" "), Icon(IconType.Separator), Text(" "), Key("At-Will"), Text(" (1/round)"))
      extract(parts) must matchCompletePower(defining(Seq(), "Fey Light", null, AtWillUsage("1/round")))
    }

    "read power with name and no keyword" in {
      val parts = List(Icon(IconType.Melee), Key(" Name "), Icon(IconType.Separator), Key("Aura"), Text(" 1"))
      extract(parts) must matchCompletePower(defining(
        name = "Name",
        keyword = null,
        usage = AuraUsage("1"),
        icons = Seq(IconType.Melee)))
    }

    "handle legacy entries as such" in {
      val parts = List(Icon(IconType.Melee), Key(" Name "), Text("(standard, at-will)"), Icon(IconType.Separator), Key("Weapon"))
      extract(parts) must matchLegacyPower(defining(
        name = "Name",
        keyword = "Weapon",
        usage = null,
        icons = Seq(IconType.Melee)) and beLegacyWithUsage("(standard, at-will)"))
    }

    "read legacy power with no keywords" in {
      //<P class="flavor alt"> <B>Spider Burst</B> (Poison, Zone) <B></B></P>
      //val ts = getBlockStream(<P class="flavor alt"> <B>Goblin Tactics</B> (immediate reaction, when missed by a melee attack; at-will) </P>)
      //println("ANALYZE THIS ****: " + ts.head)

      val parts = List(Text(" "), Key("Goblin Tactics"), Text(" (immediate reaction, ...) "))
      extract(parts) must matchLegacyPower(
        defining(Seq(), "Goblin Tactics", null, null) and
          beLegacyWithUsage("(immediate reaction, ...)"))
    }

    "read power with encounter duration" in {
      //<P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/Z3a.gif"></IMG> <B>Darkfire</B> <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG> <B>Encounter</B></P>

      val parts = List(Icon(IconType.Range), Text(" "), Key("Darkfire"), Text(" "), Icon(IconType.Separator), Text(" "), Key("Encounter"))
      extract(parts) must matchCompletePower(defining(Seq(IconType.Range), "Darkfire", null, EncounterUsage()))
    }

    "read power with recharge all in bold" in {
      //<P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/Z1a.gif"></IMG> <B>Poison Spew</B> (Poison) <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG> <B>Recharge when first bloodied</B></P>

      val parts = List(Icon(IconType.Range), Text(" "), Key("Poison Spew"), Text(" "), Icon(IconType.Separator), Text(" "), Key("Recharge when first bloodied"))
      extract(parts) must matchCompletePower(defining(Seq(IconType.Range), "Poison Spew", null, RechargeConditionalUsage("when first bloodied")))
    }
  }

  "PowerHeaderParts" should {
    "split three way" in {
      val parts = List(Icon(IconType.Melee), Icon(IconType.Range), Key("name"), Key("2"), Icon(IconType.Separator), Key("end"), Text("game"))
      extract2(parts) must breakInto(
        Seq(IconType.Melee, IconType.Range),
        List(Key("name"), Key("2")),
        List(Key("end"), Text("game")))
    }

    "split with no separator" in {
      val parts = List(Icon(IconType.Melee), Icon(IconType.Range), Key("name"), Key("2"), Key("end"), Text("game"))
      extract2(parts) must breakInto(
        Seq(IconType.Melee, IconType.Range),
        List(Key("name"), Key("2"), Key("end"), Text("game")),
        List())
    }

    "split with no icons" in {
      val parts = List(Key("name"), Key("2"), Icon(IconType.Separator), Key("end"), Text("game"))
      extract2(parts) must breakInto(
        Seq(),
        List(Key("name"), Key("2")),
        List(Key("end"), Text("game")))
    }

    "transform empty Key from MM3 entris into a fake separation" in {
      //<P class="flavor alt"> <B>Spider Burst</B> (Poison, Zone) <B></B></P>
      val parts = List(Text(" "), Key("name"), Text("end"), Key(""))
      extract2(parts) must breakInto(
        Seq(),
        List(Key("name"), Text("end")),
        List(Key("")))
    }

    "split after cleaning bad power headers" in {
      val parts = List(Key("name"), Key("2"), Icon(IconType.Separator), Key("end"), Text("game"), Break(), Text("now"))
      extract2(parts) must breakInto(
        Seq(),
        List(Key("name"), Key("2")),
        List(Key("end"), Text("game now")))
    }
  }

  private def extract(parts: List[Parser.Part with Product with Serializable]) =
    SomePowerDefinition.unapply(parts)

  private def extract2(parts: List[Parser.Part with Product with Serializable]) =
    PowerHeaderParts.unapply(parts)

  private def defining(icons: Seq[IconType.Value], name: String, keyword: String, usage: Usage) =
    (be_==(name) ^^ { t: PowerDefinition => t.name}).updateMessage("Power name mismatch: " + _) and
      (be_==(keyword) ^^ { t: PowerDefinition => t.keyword}).updateMessage("Power keyword mismatch: " + _) and
      (be_==(icons) ^^ { t: PowerDefinition => t.icons}).updateMessage("Power icons mismatch: " + _) and
      (be_==(usage) ^^ { t: PowerDefinition => t.usage}).updateMessage("Power usage mismatch: " + _)

  private def beLegacyWithUsage(e: String): Matcher[PowerDefinition] =
    beLike({ case l: LegacyPowerDefinition => l.actionUsage must_== e })

  private def matchCompletePower(m: Matcher[PowerDefinition]) = beLike[Option[PowerDefinition]]({
    case Some(p@CompletePowerDefinition(_, _, _, _)) => p must m
  })

  private def matchLegacyPower(m: Matcher[LegacyPowerDefinition]) = beLike[Option[PowerDefinition]]({
    case Some(p@LegacyPowerDefinition(_, _, _, _, _)) => p must m
  })

  private def breakInto(icons: Seq[IconType.Value], part1:Seq[Part], part2: Seq[Part]) =
     beLike[Option[(Seq[IconType.Value], List[Part], List[Part])]]({
       case Some((aIcons, p1, p2)) =>
         (icons must_== aIcons) and
           (p1 must_== part1) and
           (p2 must_== part2)
     })

}