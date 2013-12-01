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
package vcc.dnd4e.view

import org.specs2.SpecificationWithJUnit
import org.specs2.mock.Mockito
import vcc.dnd4e.view.DamageEffectEditor.Memento

class DamageEffectEditorPresenterTest extends SpecificationWithJUnit with Mockito {

  def is = s2"""
    setting memento will updated view fields ${s().e1}
    clearing memento will clear view fields ${s().e2}
  """

  case class s() {
    val (presenter, view) = makePresenter()

    def e1 = {
      presenter.setMemento(Some(Memento("damage", "condition")))

      (there was one(view).setDamageText("damage")) and
        (there was one(view).setConditionText("condition"))
    }

    def e2 = {
      presenter.setMemento(None)

      (there was one(view).setDamageText("")) and
        (there was one(view).setConditionText(""))
    }
  }

  private def makePresenter() = {
    val presenter = new DamageEffectEditorPresenter()
    val mockView = mock[DamageEffectEditor.View]
    presenter.bind(mockView)
    (presenter, mockView)
  }
}