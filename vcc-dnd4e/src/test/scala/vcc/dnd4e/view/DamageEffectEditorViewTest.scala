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

import org.uispec4j.{Window, UISpecAdapter, UISpecTestCase}
import org.mockito.Mockito._
import scala.swing.Frame

class DamageEffectEditorViewTest extends UISpecTestCase {

  private var view: DamageEffectEditor.View = null
  private val presenter: DamageEffectEditorPresenter = mock(classOf[DamageEffectEditorPresenter])

  override def setUp() {
    super.setUp()
    val panel = new DamageEffectEditor(presenter)
    view = panel
    setAdapter(new UISpecAdapter() {
      def getMainWindow: Window = {
        val frame = new Frame {
          contents = panel
        }
        new Window(frame.peer)
      }
    })
  }

  def testBasicBinding() {
    assertFalse(getMainWindow.isVisible)
    verify(presenter).bind(view)
  }

  def testSettingDamageUpdatesField() {
    view.setDamageText("some damage")
    assertThat(getMainWindow.getTextBox("dee.damage").textEquals("some damage"))
  }

  def testSettingConditionUpdatesField() {
    view.setConditionText("some condition")
    assertThat(getMainWindow.getTextBox("dee.condition").textEquals("some condition"))
  }
}