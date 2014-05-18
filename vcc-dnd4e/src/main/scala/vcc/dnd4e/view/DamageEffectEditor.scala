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

import vcc.util.swing.{AutoCompleteDictionary, AutoCompleteTextComponent, MigPanel}
import scala.swing._
import vcc.dnd4e.view.DamageEffectEditor._
import scala.swing.event.ValueChanged
import org.slf4j.LoggerFactory
import vcc.dnd4e.view.helper.DamageParser
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.common.Command.{ApplyDamage, CombatStateAction, CompoundAction, AddEffect}
import vcc.dnd4e.view.GroupFormPanel.FormValueChanged
import scala.swing.event.ButtonClicked
import vcc.dnd4e.view.GroupFormPanel.FormSave
import scala.swing.event.SelectionChanged

object DamageEffectEditor {

  object Mark extends Enumeration {
    val NoMark = Value("No Mark")
    val Regular = Value("Mark")
    val Permanent = Value("Permanent Mark")

    def toCondition(mark: Mark.Value, marker: CombatantID): Option[Condition] =
      mark match {
        case NoMark => None
        case Regular => Some(Effect.Condition.Mark(marker, permanent = false))
        case Permanent => Some(Effect.Condition.Mark(marker, permanent = true))
      }

    def formatted(mark: Mark.Value): Option[String] =
      if (mark != Mark.NoMark) Some(mark.toString) else None
  }

  sealed trait ConditionDefinition {
    def condition:String
  }

  case class BeneficialCondition(condition: String) extends ConditionDefinition
  case class HarmfulCondition(condition: String) extends ConditionDefinition

  case class EffectMemento(condition: Option[ConditionDefinition], mark: Mark.Value = Mark.NoMark, duration: DurationComboEntry = DurationComboEntry.durations.head) {
    def formatted = List(condition.map(_.condition), Mark.formatted(mark)).flatMap(x => x).mkString("; ")
  }

  case class Memento(name: Option[String], damage: Option[String],
                     effect: Option[EffectMemento] = None) {
    def asListText = s"""<html><body style="font-weight: normal"><strong>${name.getOrElse("-")}</strong><br/>&nbsp;$formattedEffect<br>&nbsp;$formattedDuration</body></html>"""

    private def formattedEffect = List(damage, effect.map(_.formatted)).flatMap(x => x).mkString("; ")

    private def formattedDuration = effect.fold("")(_.duration.toString)
  }

}

class DamageEffectEditor(panelDirector: PanelDirector) extends MigPanel("fillx", "[fill,grow]", "[][]unrel[][]unrel[][][][]unrel[][]15[]")
with ContextObserver
with GroupFormPanel.Presenter[Memento] {

  private val nameField = new TextField()
  private val damageField = new TextField()
  private val damageValueField = new TextField()
  private val conditionField = new TextField() with AutoCompleteTextComponent
  private val markCheckbox = new CheckBox("Mark")
  private val permanentMarkCheckbox = new CheckBox("Can't be Superseded")
  private val beneficialCheckbox = new CheckBox("Beneficial to target")
  private val durationCombo = new ComboBox[DurationComboEntry](DurationComboEntry.durations)
  private val applyButton = new Button(Action("Apply") {
    doApply()
  })

  private val rollButton = new Button(Action("") {
    rollDamage(max = false)
  })

  private val maxDamageButton = new Button(Action("") {
    rollDamage(max = true)
  })

  private var lastDamageValue = ""
  private var targetID: Option[UnifiedCombatantID] = None
  private var sourceID: Option[UnifiedCombatantID] = None

  init()

  listenTo(damageField, markCheckbox, conditionField, durationCombo.selection)
  reactions += {
    case ValueChanged(this.damageField) if lastDamageValue != damageField.text =>
      toggleApply()
      val damageTermOption = rollDamage(max = false)
      rollButton.enabled = damageTermOption.isDefined
      maxDamageButton.enabled = rollButton.enabled
      publish(FormValueChanged(this, damageTermOption.isDefined))
    case ButtonClicked(this.markCheckbox) =>
      adjustMarkCheckboxes()
      toggleApply()
      toggleDurationCombo()
    case ValueChanged(this.conditionField) =>
      toggleApply()
      toggleDurationCombo()
      toggleBeneficial()
    case SelectionChanged(_) =>
      applyButton.enabled = isDurationIsApplicable
  }

  private def init() {
    rollButton.icon = IconLibrary.DiceIcon
    rollButton.tooltip = "Roll damage dice"

    maxDamageButton.icon = IconLibrary.DiceMaxIcon
    maxDamageButton.tooltip = "Assign damage to maximum value of damage expression"

    nameField.name = "dee.name"
    add(createLabel("Name:"), "wrap")
    add(nameField, "gap unrel, wrap")

    damageField.name = "dee.damage"
    add(createLabel("Damage:"), "wrap")
    add(damageField, "gap unrel, split 4, grow 100")

    rollButton.name = "dee.roll"
    add(rollButton, "gap unrel, grow 0")

    maxDamageButton.name = "dee.maxDamage"
    add(maxDamageButton, "gap rel, grow 0")

    damageValueField.name = "dee.damageValue"
    damageValueField.editable = false
    add(damageValueField, "grow 30, gap rel, wrap")

    conditionField.name = "dee.condition"
    conditionField.enableAutoComplete(loadAutoCompleteDictionary())
    add(createLabel("Condition:"), "wrap")
    add(conditionField, "gap rel, wrap")

    beneficialCheckbox.name = "dee.beneficial"
    beneficialCheckbox.enabled = false
    add(beneficialCheckbox, "wrap, gapleft unrel")

    markCheckbox.name = "dee.mark"
    add(markCheckbox, "split 2, gap rel")

    permanentMarkCheckbox.enabled = false
    permanentMarkCheckbox.name = "dee.permanentMark"
    add(permanentMarkCheckbox, "wrap")

    add(createLabel("Duration:"), "wrap")
    durationCombo.name = "dee.duration"
    durationCombo.enabled = false
    add(durationCombo, "gap unrel, wrap")

    applyButton.name = "dee.apply"
    applyButton.enabled = false
    add(applyButton)
  }

  def setEntry(entry: Memento) {
    nameField.text = entry.name.getOrElse("")
    damageField.text = entry.damage.getOrElse("")
    if (entry.effect.isDefined) {
      setEffectFields(entry.effect.get)
    } else {
      clearEffectFields()
    }
    adjustMarkCheckboxes()
    toggleApply()
    toggleDurationCombo()
  }

  def getEntry: Memento = {
    Memento(
      fieldAsOption(nameField),
      fieldAsOption(damageField),
      if (fieldAsOption(conditionField).isDefined || markValue != Mark.NoMark)
        Some(EffectMemento(buildConditionOption(), mark = markValue, duration = durationCombo.selection.item))
      else
        None)
  }

  def clear() {
    damageField.text = ""
    conditionField.text = ""
    nameField.text = ""
    clearEffectFields()
    adjustMarkCheckboxes()
    toggleApply()
    toggleDurationCombo()
  }

  def isValid: Boolean = {
    if (!isWhiteSpace(damageField.text))
      DamageParser.parseDamageExpression(damageField.text).isRight
    else
      true
  }

  override def changeTargetContext(newContext: Option[UnifiedCombatantID]) {
    targetID = newContext
    toggleApply()
  }

  override def changeSourceContext(newContext: Option[UnifiedCombatantID]) {
    sourceID = newContext
    toggleApply()
  }

  private def toggleApply() {
    applyButton.enabled =
      targetID.isDefined &&
        isDamageApplicable ||
        (hasEffectDefined && isDurationIsApplicable)
  }

  private def setEffectFields(effect: EffectMemento) {
    beneficialCheckbox.selected = effect.condition.fold(false)({
      case BeneficialCondition(condition) => true
      case _ => false
    })
    conditionField.text = effect.condition.fold("")(_.condition)
    markCheckbox.selected = effect.mark != Mark.NoMark
    permanentMarkCheckbox.selected = effect.mark == Mark.Permanent
    durationCombo.selection.item = effect.duration
  }

  private def clearEffectFields() {
    conditionField.text = ""
    markCheckbox.selected = false
    permanentMarkCheckbox.selected = false
    durationCombo.selection.item = DurationComboEntry.durations.head
    beneficialCheckbox.selected = false
  }

  private def isDamageApplicable: Boolean = isValid && !isWhiteSpace(damageField.text)

  private def hasEffectDefined: Boolean =
    !isWhiteSpace(conditionField.text) || markValue != Mark.NoMark

  private def isDurationIsApplicable: Boolean =
    if (targetID.isDefined && sourceID.isDefined) {
      durationCombo.selection.item.isDefinedAt(sourceID.get, targetID.get)
    } else {
      false
    }

  private def createLabel(labelText: String): Label = {
    val label = new Label(labelText)
    label.xAlignment = Alignment.Leading
    label
  }

  private def fieldAsOption(field: TextField): Option[String] = if (isWhiteSpace(field.text)) None else Some(field.text)

  private def doApply() {
    publish(FormSave(this))
    if (sourceID.isDefined && targetID.isDefined) {
      val condition = fieldAsOption(conditionField).map(Effect.Condition.Generic(_, beneficial = beneficialCheckbox.selected))
      val source = sourceID.get
      val target = targetID.get
      val duration = durationCombo.selection.item.generate(source, target)

      val actions: List[CombatStateAction] = List(
        condition.map(AddEffect(target.combId, source.combId, _, duration)),
        Mark.toCondition(markValue, source.combId).map(AddEffect(target.combId, source.combId, _, duration)),
        fieldAsOption(damageValueField).map(value => ApplyDamage(target.combId, value.toInt))
      ).flatMap(x => x)

      panelDirector.requestAction(CompoundAction(actions))
    }
  }

  private def adjustMarkCheckboxes() {
    if (markCheckbox.selected) {
      permanentMarkCheckbox.enabled = true
    } else {
      permanentMarkCheckbox.enabled = false
      permanentMarkCheckbox.selected = false
    }
  }

  private def toggleDurationCombo() {
    durationCombo.enabled = !isWhiteSpace(conditionField.text) || markCheckbox.selected
  }

  private def toggleBeneficial() {
    beneficialCheckbox.enabled = !isWhiteSpace(conditionField.text)
  }

  private def rollDamage(max: Boolean) = {
    val damageTerm = DamageParser.parseDamageExpression(damageField.text).right.toOption
    val maxDamageOrNot: Map[String, Int] = if (max) Map("max" -> 1) else Map()
    damageValueField.text = damageTerm.fold("")(_(maxDamageOrNot).toString)
    lastDamageValue = damageField.text
    damageTerm
  }

  private def markValue: DamageEffectEditor.Mark.Value = {
    if (permanentMarkCheckbox.selected) Mark.Permanent
    else if (markCheckbox.selected) Mark.Regular
    else Mark.NoMark
  }

  private def buildConditionOption(): Option[ConditionDefinition] =
    fieldAsOption(conditionField).map {
      condition =>
        if(beneficialCheckbox.selected)  BeneficialCondition(condition) else HarmfulCondition(condition)
    }

  private def loadAutoCompleteDictionary(): AutoCompleteDictionary = {
    val logger = LoggerFactory.getLogger("startup")

    AutoCompleteDictionary.loadFromResource("/vcc/dnd4e/view/autocomplete.dict", (term, msg) => {
      logger.warn("AutoComplete[{}]: {}", Array(term, msg))
    }).getOrElse {
      logger.warn("Failed to open resource: /vcc/dnd4e/view/autocomplete.dict")
      new AutoCompleteDictionary(Nil)
    }
  }

  private def isWhiteSpace(value: String) = value.forall(_.isWhitespace)
}