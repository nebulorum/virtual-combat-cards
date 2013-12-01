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
  Setting content on presenter updates view $e1
  Remove entry and update list on remove $e2
  Remove entry that is not there should not change anything $e3
  """

  private def e1 = {
    val (_, mockView) = setupLoadedPresenter()
    there was one(mockView).setListContent(sampleContent)
  }

  private def e2 = {
    val (presenter, mockView) = setupLoadedPresenter()
    presenter.removeEntry(sampleContent(0).id)
    there was one(mockView).setListContent(sampleContent.tail)
  }

  private def e3 = {
    val (presenter, mockView) = setupLoadedPresenter()
    presenter.removeEntry(-1)
    there was one(mockView).setListContent(sampleContent)
  }

  private def setupLoadedPresenter() = {
    val presenter = new DamageEffectPresenter()
    val mockView = mock[DamageEffectPanel.View]
    presenter.bind(mockView)
    presenter.setContent(sampleContent)
    (presenter, mockView)
  }
}