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
package vcc.dndi.common

object FormattedText {
  sealed trait Part

  case class Italic(s: String) extends Part

  case class Bold(s: String) extends Part

  case class Normal(s: String) extends Part

  case class Image(src: String) extends Part

  case class Line(indent:Int, parts: Seq[Part]) {
    override def toString: String = {
      parts.map{
        _ match {
          case Italic(s) => "_%s_".format(s)
          case Bold(s) => "*%s*".format(s)
          case Image(s) => "{%s}".format(s)
          case Normal(s) => s
        }
      }.mkString(("\\t" * indent), "", "")
    }
  }

  case class Block(lines: Seq[Line]) {
    override def toString: String = {
      lines.mkString("\"", "\\n", "\"")
    }
  }

}