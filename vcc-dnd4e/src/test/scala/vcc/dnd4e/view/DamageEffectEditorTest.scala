/*
 * Copyright (C) 2014-2014 - Thomas Santana <tms@exnebula.org>
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

import org.uispec4j.{UISpecTestCase, Window, UISpecAdapter}
import scala.swing.{Reactor, Frame}
import org.uispec4j.assertion.Assertion
import vcc.dnd4e.view.DamageEffectEditor.Memento
import vcc.dnd4e.view.GroupFormPanel.FormValueChanged
import scala.swing.event.Event

class DamageEffectEditorTest extends UISpecTestCase {
  var view: DamageEffectEditor = null

  override def setUp() {
    super.setUp()
    val panel = new DamageEffectEditor
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

  def testSettingCondition_shouldSetConditionMemento() {
    getConditionField.setText("slowed")
    assertThat(mustBeEqual(Memento("", "slowed"), view.getEntry))
  }

  def testSettingDamage_shouldSetDamageMemento() {
    getDamageField.setText("1d12+2")
    assertThat(mustBeEqual(Memento("1d12+2", ""), view.getEntry))
  }

  def testSettingMemento_shouldUpdateField() {
    view.setEntry(Memento("1d10+1", "immobilized"))
    assertThat(getConditionField.textEquals("immobilized"))
    assertThat(getDamageField.textEquals("1d10+1"))
  }

  def testSettingDamageToBadInput_shouldChangeToInvalid() {
    getDamageField.setText("xx")
    assertThat(mustBeEqual(false, view.isValid))
  }

  def testSettingDamageToBadInput_shouldPublishEvent() {
    val probe = createEventProbe()
    probe.listenTo(view)
    getDamageField.setText("xx")
    assertThat(mustBeEqual(Some(false), probe.getLastValue))
  }

  def testSettingDamageToGoodInput_shouldPublishEvent() {
    val probe = createEventProbe()
    probe.listenTo(view)
    getDamageField.setText("1d14+5")
    assertThat(mustBeEqual(Some(true), probe.getLastValue))
    assertThat(mustBeEqual(true, view.isValid))
  }

  def testSettingMementoThenClearing_shouldLeaveBlank() {
    view.setEntry(Memento("2d4+2", "dead"))
    view.clear()
    assertThat(getDamageField.textIsEmpty())
    assertThat(getConditionField.textIsEmpty())
  }

  private def createEventProbe(): ReactorProbe[Boolean] = {
    val probe = new ReactorProbe[Boolean]({
      case FormValueChanged(_, value) => value
    })
    probe.listenTo(view)
    probe
  }

  private def getConditionField = getMainWindow.getTextBox("dee.condition")

  private def getDamageField = getMainWindow.getTextBox("dee.damage")

  private def mustBeEqual[T](expected: T, value: T) = new Assertion {
    def check() {
      if (expected != value)
        throw new AssertionError(s"Value: $value does not match: $expected")
    }
  }

  private class ReactorProbe[T](pf: PartialFunction[Event, T]) extends Reactor {
    private var lastValue: Option[T] = None
    reactions += {
      case event if pf.isDefinedAt(event) =>
        lastValue = Some(pf(event))
    }

    def getLastValue = lastValue
  }

}