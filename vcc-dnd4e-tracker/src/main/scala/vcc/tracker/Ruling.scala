/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.tracker

/**
 * Indicates that a specified decision is not valid.
 */
class InvalidDecisionException(msg: String) extends Exception(msg)

class UndecidedRulingException(msg: String) extends Exception(msg)

/**
 * A request for a ruling or a completed ruling.
 */
abstract class Ruling[S, D, R <: Ruling[S, D, R]] {
  val decision: Option[D]

  /**
   * Check to see if ruling refer to the same subject. This MUST consider the class and the argument
   * to avoid mixing subjects form different classes with similar arguments.
   */
  def isRulingSameSubject(otherRuling: Ruling[S, _, _]):Boolean

  def userPrompt(state: S): String

  /**
   * Determines whether the ruling has a decision or not.
   */
  def hasDecision: Boolean = decision.isDefined

  /**
   * This is the method must be overridden to generate appropriate events for a given decision. If this method is called
   * the ruling has a valid decision.
   * @param state Current state
   */
  protected def commandsFromDecision(state: S): List[Command[S]]

  /**
   * Give a ruling with a decision, generates all the commands that are a result of the ruling with that decision on
   * that state.
   * @param state Base state to generate the commands to.
   */
  def generateCommands(state: S): List[Command[S]] = {
    if (hasDecision) commandsFromDecision(state)
    else throw new UndecidedRulingException("No answer for ruling " + this)
  }

  /**
   * Create a new ruling with a decision specified. This new ruling can be used to generate commands from the decision.
   * @param decision A decision of the appropriate type
   * @return A new ruling of type R with the a valida ruling.
   */
  def withDecision(decision: D): R
}


