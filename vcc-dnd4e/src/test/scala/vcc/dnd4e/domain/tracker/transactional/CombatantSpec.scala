/**
 * Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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
//$Id$
package vcc.dnd4e.domain.tracker.transactional


import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import vcc.dnd4e.model.common.CombatantType
import vcc.dnd4e.domain.tracker.common._
import vcc.dnd4e.model.CombatantEntity
import vcc.infra.test.{TransactionalSpecification}
import org.specs.Specification

@RunWith(classOf[JUnitSuiteRunner])
class CombatantTest extends JUnit4(CombatantSpec)

object CombatantSpec extends Specification with TransactionalSpecification {
  val cid = CombatantID("A")
  val combDef = new CombatantRosterDefinition(cid, "007", CombatantEntity(null, "Bond", vcc.dnd4e.model.common.CharacterHealthDefinition(40, 10, 6), 4, CombatantType.Character, null))
  val newCombDef = new CombatantRosterDefinition(cid, "alias", CombatantEntity(null, "Elektra", vcc.dnd4e.model.common.CharacterHealthDefinition(50, 12, 7), 4, CombatantType.Character, null))
  var aCombatant: Combatant = null

  shareVariables()

  val ctx = beforeContext {
    aCombatant = new Combatant(combDef)
  }

  val damagedCtx = beforeContext {
    aCombatant = new Combatant(combDef)
    runAndCommit(trans => aCombatant.health_=(aCombatant.health.applyDamage(10))(trans))
  }

  "a base Combatant" ->- (ctx) should {
    "be properly built" in {
      aCombatant must notBeNull
      aCombatant.effects must_== EffectList(Nil)
      aCombatant.comment must_== ""
      aCombatant.health must_== HealthTracker.createTracker(aCombatant.projectHealthDef(combDef.entity.healthDef))
      aCombatant.definition must_== combDef
    }
  }

  "a transaction Combatant" ->- (ctx) should {

    "update comment" in {
      withTransaction {
        trans =>
          aCombatant.comment_=("New comment")(trans)
      } afterCommit {
        changes =>
          changes must contain(CombatantChange(cid, CombatantComment("New comment")))
          aCombatant.comment must_== "New comment"
      } afterUndo {
        changes =>
          aCombatant.comment must_== ""
          changes must contain(CombatantChange(cid, CombatantComment("")))
      } afterRedoAsInCommit
    }

    "update HealthTracker" in {
      val oldHealth = aCombatant.health
      val damaged = aCombatant.health.applyDamage(10)

      withTransaction {
        trans =>
          aCombatant.health_=(oldHealth.applyDamage(10))(trans)
      } afterCommit {
        changes =>
          aCombatant.health must_== damaged
          changes must contain(CombatantChange(cid, damaged))
      } afterUndo {
        changes =>
          aCombatant.health must_== oldHealth
          changes must contain(CombatantChange(cid, oldHealth))
      } afterRedoAsInCommit ()
    }

    "update EffectList" in {

      val baseEL = EffectList(Nil)
      val modifiedEL = baseEL.add(Effect(cid, Condition.Generic("safe"), true, Effect.Duration.SaveEnd))

      withTransaction {
        trans =>
          aCombatant.effects_=(modifiedEL)(trans)
      } afterCommit {
        changes =>
          aCombatant.effects must_== modifiedEL
          changes must contain(CombatantChange(cid, modifiedEL))
      } afterUndo {
        changes =>
          aCombatant.effects must_== baseEL
          changes must contain(CombatantChange(cid, baseEL))
      } afterRedoAsInCommit ()
    }

  }

  "aCombatant that has been damaged" ->- (damagedCtx) should {

    "update Definition while preserving health" in {
      withTransaction {
        trans =>
          aCombatant.setDefinition(newCombDef)(trans)
      } afterCommit {
        changes =>
          aCombatant.definition must_== newCombDef
          changes must contain(CombatantChange(cid, newCombDef))
          changes must contain(CombatantChange(cid, HealthTracker(40, 0, 0, 7, aCombatant.projectHealthDef(newCombDef.entity.healthDef))))
      } afterUndo {
        changes =>
          aCombatant.definition must_== combDef
          changes must contain(CombatantChange(cid, combDef))
          changes must contain(CombatantChange(cid, HealthTracker(30, 0, 0, 6, aCombatant.projectHealthDef(combDef.entity.healthDef))))
      } afterRedoAsInCommit ()
    }
  }
}