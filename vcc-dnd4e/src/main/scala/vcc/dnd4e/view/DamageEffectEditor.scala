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
import vcc.dnd4e.view.DamageEffectEditor.{Mark, Memento}
import scala.swing.event.{SelectionChanged, ValueChanged, ButtonClicked}
import org.slf4j.LoggerFactory
import vcc.dnd4e.view.GroupFormPanel.{FormSave, FormValueChanged}
import scala.Some
import vcc.dnd4e.view.helper.DamageParser
import vcc.dnd4e.tracker.common.UnifiedCombatantID

object DamageEffectEditor {

  object Mark extends Enumeration {
    val None = Value("No Mark")
    val Regular = Value("Mark")
    val Permanent = Value("Permanent Mark")
  }

  case class Memento(name: Option[String], damage: Option[String], condition: Option[String],
                     mark: Mark.Value = Mark.None,
                     duration: DurationComboEntry = DurationComboEntry.durations.head) {
    def asListText = s"""<html><body style="font-weight: normal"><strong>${name.getOrElse("-")}</strong><br/>&nbsp;$formattedEffect<br>&nbsp;$formattedDuration</body></html>"""

    private def formattedEffect = {
      val parts: List[Option[String]] = List(damage, condition, if (mark != Mark.None) Some(mark.toString) else None)
      parts.flatMap(x => x).mkString("; ")
    }

    private def formattedDuration = if (condition.isDefined || mark != Mark.None) duration.toString else ""
  }

}

class DamageEffectEditor extends MigPanel("fillx", "[fill,grow]", "[][]unrel[][]unrel[][][]unrel[][]15[]")
with ContextObserver
with GroupFormPanel.Presenter[Memento] {

  private val nameField = new TextField()
  private val damageField = new TextField()
  private val conditionField = new TextField() with AutoCompleteTextComponent
  private val markCheckbox = new CheckBox("Mark")
  private val permanentMarkCheckbox = new CheckBox("Can't be Superseded")
  private val durationCombo = new ComboBox[DurationComboEntry](DurationComboEntry.durations)
  private val applyButton = new Button(Action("Apply") {
    doApply()
  })

  private var targetID: Option[UnifiedCombatantID] = None
  private var sourceID: Option[UnifiedCombatantID] = None

  init()

  listenTo(damageField, markCheckbox, conditionField, durationCombo.selection)
  reactions += {
    case ValueChanged(this.damageField) =>
      toggleApply()
      publish(FormValueChanged(this, isValid))
    case ButtonClicked(this.markCheckbox) =>
      adjustMarkCheckboxes()
      toggleApply()
      toggleDurationCombo()
    case ValueChanged(this.conditionField) =>
      toggleApply()
      toggleDurationCombo()
    case SelectionChanged(_) =>
      applyButton.enabled = isDurationIsApplicable
  }

  private def init() {
    nameField.name = "dee.name"
    add(createLabel("Name:"), "wrap")
    add(nameField, "gap unrel, wrap")

    damageField.name = "dee.damage"
    add(createLabel("Damage:"), "wrap")
    add(damageField, "gap unrel, wrap")

    conditionField.name = "dee.condition"
    conditionField.enableAutoComplete(loadAutoCompleteDictionary())
    add(createLabel("Condition:"), "wrap")
    add(conditionField, "gap unrel, wrap")

    markCheckbox.name = "dee.mark"
    add(markCheckbox, "split 2, gap unrel")

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
    conditionField.text = entry.condition.getOrElse("")
    markCheckbox.selected = entry.mark != Mark.None
    permanentMarkCheckbox.selected = entry.mark == Mark.Permanent
    durationCombo.selection.item = entry.duration
    adjustMarkCheckboxes()
    toggleApply()
    toggleDurationCombo()
  }

  def getEntry: Memento = {
    Memento(
      fieldAsOption(nameField),
      fieldAsOption(damageField),
      fieldAsOption(conditionField),
      markValue,
      durationCombo.selection.item
    )
  }

  def clear(): Unit = {
    damageField.text = ""
    conditionField.text = ""
    nameField.text = ""
    markCheckbox.selected = false
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

  private def isDamageApplicable: Boolean = isValid && !isWhiteSpace(damageField.text)

  private def hasEffectDefined: Boolean =
    !isWhiteSpace(conditionField.text) || markValue != Mark.None

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

  private def markValue: DamageEffectEditor.Mark.Value = {
    if (permanentMarkCheckbox.selected) Mark.Permanent
    else if (markCheckbox.selected) Mark.Regular
    else Mark.None
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