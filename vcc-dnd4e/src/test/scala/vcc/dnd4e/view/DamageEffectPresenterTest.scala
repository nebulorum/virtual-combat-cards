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

class DamageEffectPresenterTest extends SpecificationWithJUnit with Mockito {

  private val sampleContent = new DamageEffectPanelTest().sampleContent

  def is = s2"""
  Setting content on presenter updates view ${s().e1}
  Remove entry and update list on remove ${s().e2}
  Remove entry that is not there should not change anything ${s().e3}
  When switching should update name on input field ${s().e4}
  When switching should clear input if entry not in content ${s().e5}
  """

  case class s() {
    val (presenter, mockView, mockEditorPresenter) = setupLoadedPresenter()

    def e1 = {
      there was one(mockView).setListContent(sampleContent)
    }

    def e2 = {
      presenter.removeEntry(sampleContent(0).id)
      (there was one(mockView).setListContent(sampleContent.tail)) and
        (there was one(mockView).setName(""))
    }

    def e3 = {
      presenter.removeEntry(-1)
      there was one(mockView).setListContent(sampleContent)
    }

    def e4 = {
      presenter.switchSelection(sampleContent(0).id)
      (there was one(mockView).setName(sampleContent(0).name)) and
        (there was one(mockEditorPresenter).setMemento(Some(sampleContent(0).damageEffect)))
    }

    def e5 = {
      presenter.switchSelection(-1)
      (there was one(mockView).setName("")) and
        (there was one(mockEditorPresenter).setMemento(None))
    }
  }

  private def setupLoadedPresenter() = {
    val mockEditor = mock[DamageEffectEditorPresenter]
    val presenter = new DamageEffectPresenter(mockEditor)
    val mockView = mock[DamageEffectPanel.View]
    presenter.bind(mockView)
    presenter.setContent(sampleContent)
    (presenter, mockView, mockEditor)
  }
}