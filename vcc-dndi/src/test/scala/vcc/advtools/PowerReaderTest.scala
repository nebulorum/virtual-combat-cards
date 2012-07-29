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

import org.specs2.mutable.SpecificationWithJUnit
import vcc.advtools.Monster._
import xml.XML
import vcc.dndi.reader.{RechargeDiceUsage, AtWillUsage}

class PowerReaderTest extends SpecificationWithJUnit {

  "read simple power" in {
    readPower("power-simple.xml") must_==
      Power("Short Sword", "Standard", AtWillUsage(0), BasicAttack("Melee"), Set("Weapon"),
        b("_Attack:_ +13 vs. AC\n_Hit:_ 1d6+8 damage"))
  }

  "read trigger" in {
    readPower("power-trigger-noaction.xml") must_==
      Power("Random Eye Ray", "No Action", AtWillUsage(0), NormalAttack("Ranged"), Set(),
        b("_Trigger:_ The trigger\n" +
          "_Effect (No Action):_ The beholder uses one random eye ray against the triggering enemy."))
  }

  "read power with range constrained and no damage" in {
    readPower("power-ranged-constrained-no-damage.xml") must_==
      Power("Luring Glare", "Minor", AtWillUsage(0), NormalAttack("Close Blast"), Set("Charm"),
        b("_Attack:_ Close Blast 10 (one creature in the blast); +22 vs. Will\n" +
          "_Hit:_ The dragon slides the target up to 3 squares."))
  }

  "read power with simple effect and requirement" in {
    readPower("power-simple-effect-requirement.xml") must_==
      Power("Eye Ray Frenzy", "Standard", RechargeDiceUsage(6), NormalAttack("Ranged"), Set(),
        b("_Requirement:_ The beholder must be bloodied\n" +
          "_Effect:_ As eye rays above, except the beholder makes three eye ray attacks."))
  }

  "read power with miss no damage" in {
    readPower("power-with-miss-no-damage.xml") must_==
      Power("Bite", "Standard", AtWillUsage(0), BasicAttack("Melee"), Set("Poison"),
        b("_Attack:_ Melee 3 (one creature); +24 vs. AC\n" +
          "_Hit:_ 3d10 + 14 damage, and ongoing 10 poison damage (save ends).\n" +
          "_Miss:_ 10 poison damage."))
  }

  "read power with attack detail and miss" in {
    readPower("power-attack-description.xml") must_==
      Power("Claw", "Standard", AtWillUsage(0), NormalAttack("Melee"), Set(),
        b("_Attack:_ Melee 3 (one or two creatures); +24 vs. AC. Some description.\n" +
          "_Hit:_ 3d8 + 13 damage, and the dragon shifts up to 2 squares.\n" +
          "_Miss:_ 1d6+2 and target pushed 1 square."))
  }

  "read power with after effect" in {
    readPower("power-with-after-effect.xml") must_==
      Power("Breath Weapon", "Standard", RechargeDiceUsage(5), NormalAttack("close blast"), Set("Poison"),
        b("_Attack:_ close blast 5 (enemies in the blast); +22 vs. Fortitude\n" +
          "_Hit:_ 2d12 + 12 poison damage, and the target is slowed and takes ongoing 15 poison damage (save ends both).\n" +
          "\t_Aftereffect:_ The target is slowed (save ends)."
        ))
  }

  "read power with secondary and failed save" in {
    readPower("power-with-failed-save.xml") must_==
      Power("Tail Sting", "Standard", AtWillUsage(0), BasicAttack("Melee"), Set("Poison"),
        b("_Attack:_ +12 vs. AC\n" +
          "_Hit:_ 1d8 + 4 damage, and the imp makes a secondary attack against the same target\n" +
          "\t_Secondary Attack:_ +10 vs. Fortitude\n" +
          "\t_Hit:_ the target takes ongoing 5 poison damage and is slowed (save ends both).\n" +
          "\t\t_First Failed Saving Throw:_ The target is immobilized instead of slowed (save ends).\n" +
          "\t\t_Second Failed Saving Throw:_ The target falls asleep for 1 hour or until woken. Poison damage from this attack does not wake a sleeping creature\n"))
  }

  "read power like beholder rays" in {
    readPower("power-beholder.xml") must_==
      Power("Eye Rays", "Standard", AtWillUsage(0), NormalAttack("Ranged"), Set(),
        b("_Effect:_ The beholder uses two of the following eye rays, using each against a different target. This attack does not provoke opportunity attacks.\n" +
          "_1. Charm Ray (charm):_ Ranged 10; +14 vs. Will; the target is dominated until the end of its next turn.\n" +
          "_2. Wounding Ray (necrotic):_ Ranged 10; +14 vs. Fortitude; 2d10 + 6 necrotic damage.\n" +
          "_3. Sleep Ray (charm):_ Ranged 10; +14 vs. Will; the target is immobilized (save ends).\n" +
          "\t_First Failed Saving Throw:_ The target is knocked unconscious instead of immobilized (save ends).\n" +
          "_4. Telekinesis Ray:_ Ranged 10; +14 vs. Fortitude; the beholder slides the target up to 4 squares.\n" +
          "_5. Slowing Ray (necrotic):_ Ranged 10; +14 vs. Reflex; 3d6 + 5 necrotic damage, and the target is slowed (save ends).\n" +
          "_6. Brilliant Ray (radiant):_ Ranged 10; +14 vs. Will; 1d6 + 5 radiant damage, and the target is blinded (save ends).\n" +
          "_7. Terror Ray (fear, psychic):_ Ranged 10; +14 vs. Will; 2d8 + 5 psychic damage, and the beholder pushes the target its speed.\n" +
          "_8. Petrifying Ray:_ Ranged 10; +14 vs. Fortitude; the target is petrified (save ends).\n" +
          "\t_Aftereffect:_ The target is immobilized (save ends).\n" +
          "_9. Death Ray (necrotic):_ Ranged 10; +14 vs. Fortitude; 2d8 + 10 necrotic damage. If the target is bloodied before or after the attack, it is also dazed (save ends).\n" +
          "\t_First Failed Saving Throw:_ The target is dazed and weakened (save ends both).\n" +
          "\t_Second Failed Saving Throw:_ The target dies.\n" +
          "_10. Disintegrate Ray:_ Ranged 10; +14 vs. Fortitude; 1d8 + 5 damage, and ongoing 10 damage (save ends)."))
  }

  private def b(text: String) = FormattedTextParser.parseBlock(text).get

  private def readPower(file: String): Power = {
    val xml = XML.load(this.getClass.getClassLoader.getResourceAsStream("vcc/advtools/" + file))
    new PowerReader(xml).read()
  }
}