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

import org.uispec4j.{Window, UISpecAdapter, UISpecTestCase}
import scala.swing.Frame
import vcc.dnd4e.tracker.common.{Duration, Effect, UnifiedSequenceTable, CombatStateBuilder}
import org.mockito.Mockito._
import vcc.dnd4e.view.DamageEffectEditor.{Mark, EffectMemento, Memento}
import vcc.dnd4e.tracker.common.Command.{AddEffect, CompoundAction}

class EffectDamagePanelTest extends UISpecTestCase with CombatStateBuilder with DamageEffectEditorFieldSelector {

  private var view: EffectDamagePanel = null

  private val director = mock(classOf[PanelDirector])

  private val combatantNames = Seq("Aº - Fighter", "1º - Goblin-mini", "2º - Goblin-mini", "3 - Goblin")

  private val mementoSword = Memento(Some("Sword"), Some("1d8 + 1"), None)

  private val mementoSlower = Memento(Some("Slower"), None,
    Some(EffectMemento(Some("Slowed"), Mark.NoMark, pickDuration("Save End"))))

  override def setUp() {
    super.setUp()
    val panel = new EffectDamagePanel(director)
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

  def testSourceComponentPresentAndEnabled() {
    assertThat(getSourceCombo.isVisible)
    assertThat(getSourceCombo.isEnabled)
  }

  def testSetSourceFromOutside_shouldUpdateCombo() {
    setupState(0)
    assertThat(getSourceCombo.contains(combatantNames: _*))
    assertThat(getSourceCombo.selectionEquals(combatantNames(0)))
  }

  def testSelectingDifferentSource_updatesPanel() {
    val uState = setupState()
    getSourceCombo.select(combatantNames(2))
    verify(director).setActiveCombatant(pickCombatant(uState, 2))
  }

  def testWithFighter_setEffectAndTarget() {
    setupState(0)
    fillFormWithMementoAndSave(mementoSword)
    assertThat(not(getGroupList.isEmpty))
    assertThat(getGroupList.isVisible)
    assertThat(getGroupList.contentEquals(mementoSword.asListText))
  }

  def testWithGoblin_setEffectAndSave() {
    setupState(1)
    fillFormWithMementoAndSave(mementoSlower)
    assertThat(getGroupList.contentEquals(mementoSlower.asListText))
  }

  def testWithGoblin_setEffectAndTarget() {
    val uState = setupState(1)
    setTarget(uState, 0)
    fillFormWithMemento(mementoSlower)
    assertThat(getApplyButton.isEnabled)
  }

  def testWithGoblin_setEffectAndApply() {
    val uState = setupState(1)
    setTarget(uState, 0)
    fillFormWithMemento(mementoSlower)
    getApplyButton.click()
    verify(director).requestAction(CompoundAction(List(
      AddEffect(uState(0).combId, uState(1).combId, Effect.Condition.Generic("Slowed", beneficial = false), Duration.SaveEnd)
    )))
  }

  def testSavingAndChangeSource_shouldClearList() {
    val uState = setupState(0)
    fillFormWithMementoAndSave(mementoSword)
    setSource(uState, 1)
    assertThat(getNameField.isVisible)
  }

  def testSettingEffectOnRepeatedCombatant_shouldShowListOnBoth() {
    val uState = setupState()
    setSource(uState, 1)
    fillFormWithMementoAndSave(mementoSword)
    assertThat(getGroupList.contentEquals(mementoSword.asListText))
    setSource(uState, 2)
    assertThat(getGroupList.contentEquals(mementoSword.asListText))
  }

  def testBouncingAroundCombatantsWithSetEffect() {
    val uState = setupState(0)
    fillFormWithMementoAndSave(mementoSword)
    setSource(uState, 1)
    fillFormWithMementoAndSave(mementoSlower)
    setSource(uState, 0)
    assertThat(getGroupList.contentEquals(mementoSword.asListText))
    setSource(uState, 1)
    assertThat(getGroupList.contentEquals(mementoSlower.asListText))
  }

  private def fillFormWithMementoAndSave(memento: Memento) {
    fillFormWithMemento(memento)
    getFormSaveButton.click()
  }

  private def fillFormWithMemento(memento: Memento) {
    memento.name.foreach(getNameField.setText)
    memento.damage.foreach(getDamageField.setText)
    memento.effect.foreach {
      effect =>
        effect.condition.foreach(getConditionField.setText)
        getDurationCombo.select(effect.duration.toString)
    }
  }

  private def setupState(sourceIndex: Int) = setupStateAndOptionallySource(Some(sourceIndex))

  private def setupState() = setupStateAndOptionallySource(None)

  private def setupStateAndOptionallySource(sourceIndex: Option[Int]) = {
    val uState = createCombatState()
    view.combatStateChanged(uState)
    sourceIndex.foreach(setSource(uState, _))
    uState
  }

  private def setTarget(state: UnifiedSequenceTable, index: Int) {
    view.changeTargetContext(pickCombatant(state, index))
  }

  private def setSource(state: UnifiedSequenceTable, index: Int) {
    view.changeSourceContext(pickCombatant(state, index))
  }

  private def pickCombatant(state: UnifiedSequenceTable, index: Int) = Some(state.elements(index).unifiedId)

  private def createCombatState() = {
    val roster = Seq(
      (Some(combA), null, entityFighter),
      (None, null, entityMinion),
      (None, null, entityMinion),
      (None, null, entityGoblin))

    val state = buildState(
      buildRoster(roster: _*), addToOrder(combA, 2, 20), addToOrder(comb1, 1, 14), addToOrder(comb2, 1, 11))

    (new UnifiedSequenceTable.Builder).build(state)
  }

  protected def pickDuration(durationDescription: String): DurationComboEntry =
    DurationComboEntry.durations.find(_.toString == durationDescription).get

  private def getSourceCombo = getMainWindow.getComboBox("edp.source")

  private def getFormSaveButton = getMainWindow.getButton("form.save")

  private def getGroupList = getMainWindow.getListBox("group.list")

}