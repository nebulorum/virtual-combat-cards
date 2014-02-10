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

import org.uispec4j.{Key, UISpecTestCase, Window, UISpecAdapter}
import scala.swing.{Reactor, Frame}
import org.uispec4j.assertion.Assertion
import vcc.dnd4e.view.DamageEffectEditor.{Mark, Memento}
import vcc.dnd4e.view.GroupFormPanel.FormValueChanged
import scala.swing.event.Event
import scala.util.Random

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
    assertThat(mustBeEqual(Memento(None, None, Some("slowed")), view.getEntry))
  }

  def testSettingDamage_shouldSetDamageMemento() {
    getDamageField.setText("1d12+2")
    assertThat(mustBeEqual(Memento(None, Some("1d12+2"), None), view.getEntry))
  }

  def testSettingDamageToBlank_shouldBeValidAndSetDamageMemento() {
    getDamageField.setText(" \t ")
    assertThat(mustBeEqual(true, view.isValid))
    assertThat(mustBeEqual(Memento(None, None, None), view.getEntry))
  }

  def testSettingName_shouldChangeMemento() {
    getNameField.setText("Power name")
    assertThat(mustBeEqual(Memento(Some("Power name"), None, None), view.getEntry))
  }

  def testSettingMemento_shouldUpdateField() {
    view.setEntry(Memento(Some("Power"), Some("1d10+1"), Some("immobilized")))
    assertThat(getNameField.textEquals("Power"))
    assertThat(getConditionField.textEquals("immobilized"))
    assertThat(getDamageField.textEquals("1d10+1"))
  }

  def testSettingEmptyMemento_shouldUpdateField() {
    view.setEntry(Memento(None, None, None))
    assertThat(getNameField.textEquals(""))
    assertThat(getConditionField.textEquals(""))
    assertThat(getDamageField.textEquals(""))
    assertThat(not(getDurationCombo.isEnabled))
  }

  def testAfterSettingCondition_shouldEnableDuration() {
    view.clear()
    getConditionField.pressKey(Key.S)
    assertThat(getDurationCombo.isEnabled)
  }

  def testAfterSettingConditionAndClearing_shouldDisableDuration() {
    view.clear()
    getConditionField.pressKey(Key.S)
    getConditionField.clear()
    assertThat(not(getDurationCombo.isEnabled))
  }

  def testAfterSettingMark_shouldEnableDuration() {
    view.clear()
    getMarkCheckbox.click()
    assertThat(getDurationCombo.isEnabled)
  }

  def testAfterSettingMark_shouldEnableDuration_ThenDisabledIfCleared() {
    view.clear()
    getMarkCheckbox.click()
    getMarkCheckbox.click()
    assertThat(not(getDurationCombo.isEnabled))
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
    view.setEntry(Memento(Some("Name"), Some("2d4+2"), Some("dead"),Mark.Permanent))
    view.clear()
    assertThat(getNameField.textIsEmpty())
    assertThat(getDamageField.textIsEmpty())
    assertThat(getConditionField.textIsEmpty())
    assertThat(not(getMarkCheckbox.isSelected))
    assertThat(not(getPermanentMarkCheckbox.isSelected))
  }

  def testCondition_shouldHaveAutoComplete() {
    getConditionField.pressKey(Key.S)
    getConditionField.pressKey(Key.L)
    getConditionField.pressKey(Key.ENTER)
    assertThat(getConditionField.textContains("slowed"))
    getConditionField.pressKey(Key.A)
    getConditionField.pressKey(Key.N)
    getConditionField.pressKey(Key.ENTER)
    assertThat(getConditionField.textContains("slowed and"))
  }

  def testSelectMarkCheck_shouldEnabledPermanent() {
    assertThat(getMarkCheckbox.isEnabled)
    assertThat(not(getPermanentMarkCheckbox.isEnabled))

    getMarkCheckbox.click()
    assertThat(getMarkCheckbox.isEnabled)
  }

  def testUnSelectMarkCheck_shouldDisableAndDeselectPermanent() {
    getMarkCheckbox.click()
    getPermanentMarkCheckbox.click()
    getMarkCheckbox.click()

    assertThat(not(getMarkCheckbox.isSelected))
    assertThat(not(getPermanentMarkCheckbox.isSelected))
    assertThat(not(getPermanentMarkCheckbox.isEnabled))
  }

  def testSettingConditionOrMark_shouldEnableDuration() {
    view.clear()
    getMarkCheckbox.click()
    getConditionField.clear()
    assertThat(getDurationCombo.isEnabled)

    getMarkCheckbox.click()
    getConditionField.setText("one")
    assertThat(getDurationCombo.isEnabled)

    getConditionField.clear()
    assertThat(not(getDurationCombo.isEnabled))
  }

  def testMarkSelection_shouldChangeMemento() {
    getMarkCheckbox.click()
    assertThat(mustBeEqual(Memento(None, None, None, Mark.Regular), view.getEntry))

    getPermanentMarkCheckbox.click()
    assertThat(mustBeEqual(Memento(None, None, None, Mark.Permanent), view.getEntry))

    getMarkCheckbox.click()
    assertThat(mustBeEqual(Memento(None, None, None, Mark.None), view.getEntry))
  }

  def testSetMementoMark_shouldUpdateMarkCheckbox() {
    view.setEntry(Memento(None, None, None, Mark.Regular))
    assertThat(getMarkCheckbox.isSelected)
    assertThat(not(getPermanentMarkCheckbox.isSelected))
    assertThat(getPermanentMarkCheckbox.isEnabled)
  }

  def testSetMementoPermanentMark_shouldUpdateMarkCheckbox() {
    view.setEntry(Memento(None, None, None, Mark.Permanent))
    assertThat(getMarkCheckbox.isSelected)
    assertThat(getPermanentMarkCheckbox.isSelected)
    assertThat(getPermanentMarkCheckbox.isEnabled)
  }

  def testSetMementoPermanentMarkAndBack_shouldUpdateMarkCheckbox() {
    view.setEntry(Memento(None, None, None, Mark.Permanent))
    view.setEntry(Memento(None, None, None, Mark.None))
    assertThat(not(getMarkCheckbox.isSelected))
    assertThat(not(getPermanentMarkCheckbox.isSelected))
    assertThat(not(getPermanentMarkCheckbox.isEnabled))
  }

  def testDurationCombo_shouldHaveAllDurations() {
    assertThat(getDurationCombo.contains(DurationComboEntry.durations.map(_.toString): _*))
  }

  def testDurationComboSelection_shouldUpdateMemento() {
    val option = pickEntry(DurationComboEntry.durations)
    getDurationCombo.select(option.toString)
    assertThat(mustBeEqual(Memento(None, None, None, duration = option), view.getEntry))
  }

  def testDurationComboSelection_shouldBeSetByMemento() {
    val option = pickEntry(DurationComboEntry.durations)
    view.setEntry(Memento(None, None, None, duration = option))
    assertThat(getDurationCombo.selectionEquals(option.toString))
  }

  private def pickEntry[T](list: Seq[T]) = list(Random.nextInt(list.size))

  private def createEventProbe(): ReactorProbe[Boolean] = {
    val probe = new ReactorProbe[Boolean]({
      case FormValueChanged(_, value) => value
    })
    probe.listenTo(view)
    probe
  }

  private def getConditionField = getMainWindow.getTextBox("dee.condition")

  private def getNameField = getMainWindow.getTextBox("dee.name")

  private def getDamageField = getMainWindow.getTextBox("dee.damage")

  private def getMarkCheckbox = getMainWindow.getCheckBox("dee.mark")

  private def getPermanentMarkCheckbox = getMainWindow.getCheckBox("dee.permanentMark")

  private def getDurationCombo = getMainWindow.getComboBox("dee.duration")

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