/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.view.ruling

import org.uispec4j.interception.{WindowHandler, WindowInterceptor}
import org.uispec4j.{Window, Trigger, UISpecTestCase}
import vcc.dnd4e.tracker.event._
import vcc.dnd4e.tracker.common.Effect.Condition
import vcc.dnd4e.tracker.command._
import javax.swing.JLabel
import org.uispec4j.assertion.Assertion
import org.junit.Assert
import org.uispec4j.finder.ComponentFinder
import vcc.tracker.{RulingContext, Command, Ruling}
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.ruling._
import vcc.dnd4e.tracker.event.AlterDamageIndicationEvent.Reduce

class RulingPromptTest extends UISpecTestCase with SampleStateData {
  private val effectNameOnA = "something bad on A"
  private val effectNameOnB = "something bad on B -> worst on B / horrible on B"
  private val nameCombA = "[Aº] Goblin"
  private val nameCombANoInit = "[A] Goblin"
  private val nameCombB = "[Bº] Fighter"
  private val ongoingDescription = "Ongoing 5 fire"
  private val regenerateDescription = "Regen 2 while bloodied"

  private val commandEndRoundB = EndRoundCommand(ioiB0)
  private val commandEndRoundA = EndRoundCommand(ioiA0)
  private val commandStartRoundB = StartRoundCommand(ioiB0)

  private val state = CombatState.empty.transitionWith(List(
    AddCombatantEvent(Some(combA), null, goblinEntity),
    AddCombatantEvent(Some(combB), null, pcEntity),
    AddCombatantToOrderEvent(InitiativeDefinition(combA, 0, List(15))),
    AddCombatantToOrderEvent(InitiativeDefinition(combB, 1, List(10))),
    StartCombatEvent,
    AddEffectEvent(combA, combB, Condition.Generic(effectNameOnA, beneficial = false), Duration.EndOfEncounter),
    AddEffectEvent(combB, combA, Condition.Generic(effectNameOnB, beneficial = false), Duration.EndOfEncounter),
    AddEffectEvent(combB, combA, Condition.Generic(ongoingDescription + " and slowed", beneficial = false), Duration.EndOfEncounter),
    AddEffectEvent(combB, combA, Condition.Generic(regenerateDescription + " and +2 atk", beneficial = true), Duration.EndOfEncounter)
  ))

  override def setUp() {
    super.setUp()
  }

  private def createAndShowDialog(rulingContext: RulingContext[CombatState]): WindowInterceptor = {
    WindowInterceptor.init(new Trigger {
      def run() {
        RulingPrompt.promptUser(rulingContext)
      }
    })
  }

  def testStartRoundTitle() {
    val context = RulingContext(state, StartRoundCommand(ioiA0), Nil)
    createAndShowDialog(context).process(validateTitleAndCancel(nameCombA + " - Start Round")).run()
  }

  def testEndRoundTitle() {
    val context = RulingContext(state, EndRoundCommand(ioiB0), Nil)
    createAndShowDialog(context).process(validateTitleAndCancel(nameCombB + " - End Round")).run()
  }

  def testApplyDamageTitle() {
    val nState = state.transitionWith(List(SetDamageIndicationEvent(combA, 14)))
    val context = RulingContext(nState, ApplyDamageCommand, Nil)
    createAndShowDialog(context).process(validateTitleAndCancel(nameCombANoInit + " - Taking 14 damage")).run()
  }

  def windowContainsLabel(window: Window, expected: String): Assertion = {
    new Assertion {
      def check() {
        val finder = new ComponentFinder(window.getAwtContainer)
        finder.getComponent(expected, Array(classOf[JLabel]), "JLabel")
      }
    }
  }

  def testSustainEffect() {
    val controller = dialogController(makeContext(commandEndRoundB, makeSustainEffectList(eidA1)))
    val expectedTitle = nameCombB + " - Sustain effect: " + effectNameOnA
    controller.showDialogAndProcess(expectedTitle, "Sustain")
    Assert.assertEquals(List(SustainEffectRuling(eidA1, Some(SustainEffectRulingResult.Sustain))), controller.collectAnswer)
  }

  def testNotSustainEffect() {
    val controller = dialogController(makeContext(commandEndRoundA, makeSustainEffectList(eidB1)))
    val expectedTitle = nameCombA + " - Sustain effect: " + effectNameOnB
    controller.showDialogAndProcess(expectedTitle, "Cancel")
    Assert.assertEquals(List(SustainEffectRuling(eidB1, Some(SustainEffectRulingResult.Cancel))), controller.collectAnswer)
  }

  def testSustainEffect_selectAndCancel() {
    val controller = dialogController(makeContext(commandEndRoundA, makeSustainEffectList(eidB1)))
    controller.processWith(new WindowHandler() {
      def process(window: Window): Trigger = {
        window.getRadioButton("Sustain").click()
        window.getButton("Cancel").triggerClick()
      }
    })
    Assert.assertEquals(Nil, controller.collectAnswer)
  }

  def testSaveRuling_thenSaved() {
    val controller = dialogController(makeContext(commandEndRoundB, makeSaveRulingList(eidA1)))
    val expectedTitle = nameCombB + " - Save against: " + effectNameOnA
    controller.showDialogAndProcess(expectedTitle, "Saved")
    Assert.assertEquals(List(SaveRuling(eidA1, Some(SaveRulingResult.Saved))), controller.collectAnswer)
  }

  def testSaveRuling_thenFail() {
    val controller = dialogController(makeContext(commandEndRoundA, makeSaveRulingList(eidB1)))
    val expectedTitle = nameCombA + " - Save against: " + effectNameOnB
    controller.showDialogAndProcess(expectedTitle, "Failed")
    Assert.assertEquals(List(SaveRuling(eidB1, Some(SaveRulingResult.Failed))), controller.collectAnswer)
  }

  def testSaveVersusDeath_thenFail() {
    val controller = dialogController(makeContext(commandEndRoundA, makeSaveVersusDeathRulingList(combA)))
    val expectedTitle = nameCombA + " - Save versus Death"
    controller.showDialogAndProcess(expectedTitle, "Failed")
    Assert.assertEquals(saveVersusDeathResult(combA, SaveVersusDeathResult.Failed), controller.collectAnswer)
  }

  def testSaveVersusDeath_thenPass() {
    val controller = dialogController(makeContext(commandEndRoundA, makeSaveVersusDeathRulingList(combA)))
    val expectedTitle = nameCombA + " - Save versus Death"
    controller.showDialogAndProcess(expectedTitle, "Saved")
    Assert.assertEquals(saveVersusDeathResult(combA, SaveVersusDeathResult.Saved), controller.collectAnswer)
  }

  def testSaveVersusDeath_thenPassAndHeal() {
    val controller = dialogController(makeContext(commandEndRoundA, makeSaveVersusDeathRulingList(combA)))
    val expectedTitle = nameCombA + " - Save versus Death"
    controller.showDialogAndProcess(expectedTitle, "Saved and heal (1 HP)")
    Assert.assertEquals(saveVersusDeathResult(combA, SaveVersusDeathResult.SaveAndHeal), controller.collectAnswer)
  }

  def testSaveSpecial_thenSaved() {
    val controller = dialogController(makeContext(commandEndRoundA, makeSaveSpecialRulingList(eidB1)))
    val expectedTitle = nameCombA + " - Save against: " + effectNameOnB
    controller.showDialogAndProcess(expectedTitle, "Saved")
    Assert.assertEquals(List(SaveSpecialRuling(eidB1, Some(SaveSpecialRulingResult.Saved))), controller.collectAnswer)
  }

  def testSaveSpecial_thenFailedAndChange() {
    val controller = dialogController(makeContext(commandEndRoundA, makeSaveSpecialRulingList(eidB1)))
    val expectedTitle = nameCombA + " - Save against: " + effectNameOnB
    val progression = "worst on B -> horrible on B"
    controller.showDialogAndProcess(expectedTitle, "Failed and change to: " + progression)
    Assert.assertEquals(List(SaveSpecialRuling(eidB1, Some(SaveSpecialRulingResult.Changed(progression)))), controller.collectAnswer)
  }

  def testRegenerateRuling_skip() {
    val controller = dialogController(makeContext(commandStartRoundB, makeRegenerationList(eidB3)))
    val expectedTitle = nameCombB + " - Regeneration: " + regenerateDescription
    controller.showDialogAndProcess(expectedTitle, "Skip")
    Assert.assertEquals(List(RegenerationRuling(eidB3, Some(0))), controller.collectAnswer)
  }

  def testRegenerateRuling_regen() {
    val controller = dialogController(makeContext(commandStartRoundB, makeRegenerationList(eidB3)))
    val expectedTitle = nameCombB + " - Regeneration: " + regenerateDescription
    controller.showDialogAndProcess(expectedTitle, "Regenerate 2 HP")
    Assert.assertEquals(List(RegenerationRuling(eidB3, Some(2))), controller.collectAnswer)
  }

  def testOngoingRuling_skip() {
    val controller = dialogController(makeContext(commandStartRoundB, makeOngoingList(eidB2)))
    val expectedTitle = nameCombB + " - Ongoing Damage: " + ongoingDescription
    controller.showDialogAndProcess(expectedTitle, "Skip")
    Assert.assertEquals(List(OngoingDamageRuling(eidB2, Some(0))), controller.collectAnswer)
  }

  def testResist_skip() {
    val controller = dialogController(makeDamageContext(ApplyDamageCommand, makeResistList()))
    val expectedTitle = "Resist: 5 fire"
    controller.showDialogAndProcess(expectedTitle, "Skip")
    Assert.assertEquals(List(AlterDamageRuling("Resist: 5 fire", Reduce(5), Some(false))), controller.collectAnswer)
  }

  def testResist_apply() {
    val controller = dialogController(makeDamageContext(ApplyDamageCommand, makeResistList()))
    val expectedTitle = "Resist: 5 fire"
    controller.showDialogAndProcess(expectedTitle, "Apply")
    Assert.assertEquals(List(AlterDamageRuling("Resist: 5 fire", Reduce(5), Some(true))), controller.collectAnswer)
  }

  def testOngoingRuling_applyDamage() {
    val controller = dialogController(makeContext(commandStartRoundB, makeOngoingList(eidB2)))
    val expectedTitle = nameCombB + " - Ongoing Damage: " + ongoingDescription
    controller.showDialogAndProcess(expectedTitle, "Take 5 damage")
    Assert.assertEquals(List(OngoingDamageRuling(eidB2, Some(5))), controller.collectAnswer)
  }

  def testProgressionHelper() {
    Assert.assertEquals("b -> c", RulingPrompt.buildEffectProgression("a/b/c"))
    Assert.assertEquals("a", RulingPrompt.buildEffectProgression("a"))
    Assert.assertEquals("b -> c", RulingPrompt.buildEffectProgression("a->b/c"))
  }

  private def saveVersusDeathResult(a: CombatantID, value: SaveVersusDeathResult.Value) = {
    List(SaveVersusDeathRuling(a, Some(value)))
  }

  private def makeContext(command: Command[CombatState], rulings: List[Ruling[CombatState, _, _]]) = {
    RulingContext(state, command, rulings)
  }

  private def makeDamageContext(command: Command[CombatState], rulings: List[Ruling[CombatState, _, _]]) = {
    RulingContext(state.transitionWith(List(SetDamageIndicationEvent(combA, 15))), command, rulings)
  }

  private def makeSaveRulingList(id: EffectID): List[Ruling[CombatState, _, _]] = {
    List(SaveRuling(id, None))
  }

  private def makeRegenerationList(id: EffectID): List[Ruling[CombatState, _, _]] = {
    List(RegenerationRuling(id, None))
  }

  private def makeOngoingList(id: EffectID): List[Ruling[CombatState, _, _]] = {
    List(OngoingDamageRuling(id, None))
  }

  private def makeResistList(): List[Ruling[CombatState, _, _]] = {
    List(AlterDamageRuling("Resist: 5 fire", Reduce(5), None))
  }

  private def makeSaveSpecialRulingList(id: EffectID): List[Ruling[CombatState, _, _]] = {
    List(SaveSpecialRuling(id, None))
  }

  private def makeSustainEffectList(id: EffectID): List[Ruling[CombatState, _, _]] = {
    List(SustainEffectRuling(id, None))
  }

  private def makeSaveVersusDeathRulingList(id: CombatantID): List[Ruling[CombatState, _, _]] = {
    List(SaveVersusDeathRuling(id, None))
  }

  private def validateTitleAndCancel(title: String): WindowHandler = {
    new WindowHandler() {
      def process(window: Window): Trigger = {
        assertTrue(window.titleEquals(title))
        window.getButton("Cancel").triggerClick()
      }
    }
  }

  private case class dialogController(context: RulingContext[CombatState]) {
    private var ret: List[Ruling[CombatState, _, _]] = Nil

    private def showDialog(): WindowInterceptor = {
      WindowInterceptor.init(new Trigger {
        def run() {
          ret = RulingPrompt.promptUser(context)
        }
      })
    }

    def showDialogAndProcess(expectedPanelTitle: String, button: String) {
      showDialog().process(new WindowHandler() {
        def process(window: Window): Trigger = {
          assertTrue(windowContainsLabel(window, expectedPanelTitle))
          window.getRadioButton(button).click()
          window.getButton("Ok").triggerClick()
        }
      }).run()
    }

    def processWith(windowHandler: WindowHandler) {
      showDialog().process(windowHandler).run()
    }

    def collectAnswer = ret
  }

}