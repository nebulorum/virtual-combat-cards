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

trait Action[S] {

  protected def singleCommand(command: Command[S]): CommandStream[S] = CommandStream(command)

  def createCommandStream(): CommandStream[S]
}

trait Command[S] {

  def generateEvents(state: S): List[Event[S]]

  def requiredRulings(state: S): List[Ruling[S, _, _]] = Nil
}

trait Event[S] {
  def transition(iState: S): S
}

class IllegalActionException(msg: String) extends RuntimeException(msg)