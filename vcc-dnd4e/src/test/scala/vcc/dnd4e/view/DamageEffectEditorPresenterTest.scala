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
package vcc.dnd4e.view

import org.specs2.SpecificationWithJUnit
import org.specs2.mock.Mockito
import vcc.dnd4e.view.DamageEffectEditor.Memento
import scala.util.Random

class DamageEffectEditorPresenterTest extends SpecificationWithJUnit with Mockito {

  def is = s2"""
    setting memento will updated view fields ${s().e1}
    clearing memento will clear view fields ${s().e2}
    setting a valid damage should make form valid ${s().e3}
    setting a invalid damage should make form invalid ${s().e31}
    getEntry will collect fields form view ${s().e4}
  """

  case class s() {
    val (presenter, view) = makePresenter()

    def e1 = {
      presenter.setEntry(Memento("damage", "condition"))

      (there was one(view).setDamageText("damage")) and
        (there was one(view).setConditionText("condition"))
    }

    def e2 = {
      presenter.clear()

      (there was one(view).setDamageText("")) and
        (there was one(view).setConditionText(""))
    }

    def e3 = {
      val dice: String = genDice()
      println("Dice: " + dice)
      (presenter.validateDamage(dice) must beTrue) and (presenter.isValid must beTrue)
    }

    def e31 = {
      (presenter.validateDamage("xx" + System.currentTimeMillis) must beFalse) and
        (presenter.isValid must beFalse)
    }

    def e4 = {
      view.getConditionText returns "Condition"
      view.getDamageText returns "2d8+7"

      presenter.getEntry must_== Memento("Condition", "2d8+7")
    }
  }

  private def makePresenter() = {
    val presenter = new DamageEffectEditorPresenter()
    val mockView = mock[DamageEffectEditor.View]
    presenter.bind(mockView)
    (presenter, mockView)
  }

  private def genDice() = s"${random()}d${random()}+${random()}"

  private def random():Int = Random.nextInt(10) + 1
}