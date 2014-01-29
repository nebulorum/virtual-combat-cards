/*
 * Copyright (C) 2008-2014 - Thomas Santana <tms@exnebula.org>
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

import dnd.UnifiedCombatantActionTransfer
import helper.DamageParser
import swing._
import swing.event._
import vcc.util.swing.{MigPanel, ClickButtonAction, KeystrokeContainer}
import vcc.util.swing.KeystrokeBinder
import vcc.infra.docking._

import vcc.dnd4e.tracker.common.Command._
import vcc.util.swing.dnd.{DragAndDropSource, DragAndDropController}
import vcc.dnd4e.tracker.common.{UnifiedCombatantID, UnifiedCombatant, CombatantID}

class DamageCommandPanel(val director: PanelDirector)
  extends MigPanel("ins 2", "[fill][fill][fill][fill]", "") with KeystrokeContainer
  with ContextObserver with ScalaDockableComponent with SimpleCombatStateObserver {
  val dockID = DockID("damage")

  val dockTitle = "Health Change"

  private val damage = new TextField
  damage.columns = 3
  damage.enabled = false

  val dockFocusComponent = damage.peer

  private var target: Option[UnifiedCombatantID] = None

  private val badColor = new java.awt.Color(255, 228, 196)
  damage.background = badColor

  private val damage_btn = new Button("Damage")

  private def dispatchAction(tgt: UnifiedCombatant, director: PanelDirector, term: DamageParser.Term, builder: (CombatantID, Int) => CombatStateAction) {
    val cinfo = Map(
      "b" -> tgt.health.base.totalHP / 2,
      "s" -> tgt.health.base.totalHP / 4
    )
    val value = term.apply(cinfo)
    if (value > 0) {
      director requestAction builder(tgt.combId, value)
    }
  }

  private def makeAction(msg: String, term: DamageParser.Term, builder: (CombatantID, Int) => CombatStateAction) = {
    new UnifiedCombatantActionTransfer(msg, {
      case tgt =>
        dispatchAction(tgt, director, term, builder)
    })
  }

  damage_btn.tooltip = "Apply damage to selected combatant (shortcut Alt-D), drag to table to apply damage to combatant"

  private val heal_btn = new Button("Heal")
  heal_btn.tooltip = "Heal selected combatant (shortcut Alt-H), drag to table to heal combatant"

  private val temp_btn = new Button("Set Temporary")
  temp_btn.tooltip = "Set Temporary hitpoints on selected combatant; will keep highest value (shortcut Alt-T), drag to table to set temporary hit point combatant"

  private val death_btn = new Button("Fail Death Save")
  private val undie_btn = new Button("\"undie\"")
  undie_btn.tooltip = "Use this button to bring a dead combatant back to dying state. This will clear death strikes."
  private val controls = List(damage, damage_btn, heal_btn, temp_btn, death_btn, undie_btn)
  private val damageRelButton = List(damage_btn, heal_btn, temp_btn)

  private var damageEquation: Option[DamageParser.Term] = None

  add(new Label("Hit Points:"))
  add(damage, "wrap")
  add(damage_btn, "skip 1")
  add(heal_btn)
  add(temp_btn, "wrap")
  add(undie_btn, "skip 1,align left")
  add(death_btn, "align left,span 2")
  xLayoutAlignment = java.awt.Component.LEFT_ALIGNMENT
  for (x <- controls) listenTo(x)
  listenTo(damage)
  changeTargetContext(None)

  reactions += {
    case ValueChanged(this.damage) =>
      damageEquation = DamageParser.parseSymbolicExpression(damage.text).right.toOption
      enableDamageControls(damageEquation.isDefined)
    case FocusGained(this.damage, other, temporary) =>
      director.setStatusBarMessage("Enter equation with: + - * / and parenthesis and variable: 's' for surge value ; 'b' for bloody value")
      damage.selectAll()
    case FocusLost(this.damage, other, temp) =>
      director.setStatusBarMessage("")

    case ButtonClicked(this.death_btn) =>
      director requestAction FailDeathSave(target.get.combId)

    case ButtonClicked(this.undie_btn) =>
      director requestAction RevertDeath(target.get.combId)

    case ButtonClicked(button) if damageEquation.isDefined =>
      val tgt = combatState.combatantOption(target).get
      dispatchAction(tgt, director, damageEquation.get, button match {
        case this.damage_btn => (cid, hp) => ApplyDamage(cid, hp)
        case this.heal_btn => (cid, hp) => HealDamage(cid, hp)
        case this.temp_btn => (cid, hp) => SetTemporaryHP(cid, hp)
      })
  }

  val dc = new DragAndDropController(IconLibrary.MiniWand)
  dc.enableDragToCopy(DragAndDropSource.fromButton(damage_btn, () => {
    makeAction("Apply " + damage.text + " hit points of damage", damageEquation.get, (cid, hp) => ApplyDamage(cid, hp)).toTransferable
  }))
  dc.enableDragToCopy(DragAndDropSource.fromButton(heal_btn, () => {
    makeAction("Heal " + damage.text + " hit points", damageEquation.get, (cid, hp) => HealDamage(cid, hp)).toTransferable
  }))
  dc.enableDragToCopy(DragAndDropSource.fromButton(temp_btn, () => {
    makeAction("Set " + damage.text + " temporary hit points ", damageEquation.get, (cid, hp) => SetTemporaryHP(cid, hp)).toTransferable
  }))

  def enableDamageControls(enable: Boolean) {
    damage.background = if (enable) java.awt.Color.white else badColor
    for (x <- damageRelButton) x.enabled = enable
  }


  override def changeTargetContext(newContext: Option[UnifiedCombatantID]) {
    target = newContext
    controls map (x => x.enabled = target.isDefined)
    enableDamageControls(damageEquation != null)
  }

  def getRootPane = this.peer.getRootPane

  def registerKeystroke() {
    KeystrokeBinder.bindKeystrokeAction(damage_btn, bindToRootPane = true, KeystrokeBinder.FocusCondition.WhenWindowFocused, "alt D", new ClickButtonAction("health.damage", damage_btn))
    KeystrokeBinder.bindKeystrokeAction(heal_btn, bindToRootPane = true, KeystrokeBinder.FocusCondition.WhenWindowFocused, "alt H", new ClickButtonAction("health.heal", heal_btn))
    KeystrokeBinder.bindKeystrokeAction(temp_btn, bindToRootPane = true, KeystrokeBinder.FocusCondition.WhenWindowFocused, "alt T", new ClickButtonAction("health.settemphp", temp_btn))
  }
}