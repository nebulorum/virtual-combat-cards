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
package vcc.dnd4e.view.compendium

import org.uispec4j.UISpecTestCase
import vcc.dnd4e.view.{DamageEffectEditorFieldSelector, DamageEffectEditorCommon}
import vcc.dnd4e.tracker.common.{UnifiedCombatantID, Effect}
import org.mockito.Mockito._
import vcc.dnd4e.view.DamageEffectEditor.{EffectMemento, Mark, Memento}
import org.mockito.Matchers
import vcc.dnd4e.tracker.common.Command.{CombatStateAction, CompoundAction, AddEffect}

class DamageEffectEditorApplyTest extends UISpecTestCase with DamageEffectEditorCommon with DamageEffectEditorFieldSelector {

  def testSingleConditionOnRoundBoundDuration() {
    setMementoApplyAndVerify(ucAO, ucBO, Memento(None, None, Some(EffectMemento(Some("Stunned"), duration = pickDuration("End of source's next turn")))))
  }

  def testSingleConditionOnStaticDuration() {
    setMementoApplyAndVerify(ucAO, ucBO, Memento(None, None, Some(EffectMemento( Some("Slowed"), duration = pickDuration("Save End")))))
  }

  def testConditionAndMarkOnStaticDuration() {
    setMementoApplyAndVerify(ucAO, ucBO, Memento(None, None, Some(EffectMemento( Some("Dazed"), Mark.Permanent, pickDuration("Save End")))))
  }

  def testOnlyMarkWithRoundBoundDuration() {
    setMementoApplyAndVerify(ucBO, ucAO, Memento(None, None, Some(EffectMemento(None, Mark.Regular, pickDuration("Start of target's next turn")))))
  }
  
  def setMementoApplyAndVerify(source: UnifiedCombatantID, target: UnifiedCombatantID, memento: Memento) {
    setSourceAndTarget(source, target)

    view.setEntry(memento)

    getApplyButton.click()

    val effects:List[CombatStateAction] = List(
      makeCondition(source, target, memento),
      makeMark(source, target, memento)
    ).flatMap(x => x)
    verify(tracker).requestAction(Matchers.eq(CompoundAction(effects)))
  }

  private def makeCondition(source: UnifiedCombatantID, target: UnifiedCombatantID, memento: Memento):Option[CombatStateAction] =
    memento.effect.flatMap(_.condition).map(
      condition =>
        AddEffect(
          target.combId, source.combId,
          Effect.Condition.Generic(condition, beneficial = false),
          memento.effect.get.duration.generate(source, target)))

  private def makeMark(source: UnifiedCombatantID, target: UnifiedCombatantID, memento: Memento):Option[CombatStateAction] =
    memento.effect.flatMap(x => Mark.toCondition(x.mark, source.combId)).
      map(condition => AddEffect(target.combId, source.combId, condition, memento.effect.get.duration.generate(source, target)))
}