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
package vcc.dnd4e.view.helper

import scala.util.parsing.combinator._
import vcc.dnd4e.util.DiceBag

object DamageParser extends JavaTokenParsers {

  abstract class Term {
    def simplify(): Term

    def apply(definitions: Map[String, Int]): Int
  }

  case class NumberTerm(v: Int) extends Term {
    def simplify(): Term = this

    def apply(definitions: Map[String, Int]): Int = v
  }

  case class SymbolTerm(s: String) extends Term {
    def simplify(): Term = this

    def apply(definitions: Map[String, Int]): Int = definitions.getOrElse(s, 0)
  }

  case class DiceTerm(count: Int, size: Int) extends Term {
    def simplify(): Term = this

    def apply(definitions: Map[String, Int]): Int =
      if (definitions.isDefinedAt("max"))
        count * size
      else
        (1 to count).map(_ => DiceBag.D(size)).sum
  }

  case class Op(op: String, l: Term, r: Term) extends Term {
    def simplify(): Term = {
      (l.simplify(), r.simplify()) match {
        case (NumberTerm(lv), NumberTerm(lr)) =>
          op match {
            case "*" => NumberTerm(lv * lr)
            case "+" => NumberTerm(lv + lr)
            case "/" => NumberTerm(lv / lr)
            case "-" => NumberTerm(lv - lr)
          }
        case (nl, nr) => Op(op, nl, nr)
      }
    }

    def apply(definitions: Map[String, Int]): Int = {
      op match {
        case "+" => l(definitions) + r(definitions)
        case "-" => l(definitions) - r(definitions)
        case "*" => l(definitions) * r(definitions)
        case "/" => l(definitions) / r(definitions)
      }
    }

    override def toString: String = s"( $l $op $r )"
  }

  def reduceListOfOp(list: ~[Term, List[~[String, Term]]]): Term = {
    def reduceOp(l: Term, opRepeat: List[~[String, Term]]): Term = {
      opRepeat match {
        case Nil => l
        case op ~ r :: rest => reduceOp(Op(op, l, r), rest)
      }
    }
    reduceOp(list._1, list._2)
  }

  def expr(allowedFactors: Parser[Term]): Parser[Term] = term(allowedFactors) ~ rep("+" ~ term(allowedFactors) | "-" ~ term(allowedFactors)) ^^ { reduceListOfOp }

  def term(allowedFactors: Parser[Term]): Parser[Term] = allowedFactors ~ rep("*" ~ allowedFactors | "/" ~ allowedFactors) ^^ { reduceListOfOp }

  private def factor: Parser[Term] = number | symbol | parenthesis(factor)

  private def diceFactor: Parser[Term] = dice | number | parenthesis(diceFactor)

  private def parenthesis(allowedFactors: Parser[Term]) = "(" ~ expr(allowedFactors) ~ ")" ^^ { case "(" ~ e ~ ")" => e }
  private val diceMatcher = """(\d*)d(\d+)""".r

  private val dice: Parser[Term] = diceMatcher ^^ {
    case diceMatcher(n, s) => DiceTerm(if (n == "") 1 else n.toInt, s.toInt)
  }

  private val number: Parser[Term] = wholeNumber ^^ { x => NumberTerm(x.toInt)}

  private val symbol: Parser[Term] = """[sbSB]""".r ^^ { s => SymbolTerm(s.toLowerCase) }

  def parseSymbolicExpression(str: String) = parseToEither(str, factor)

  def parseDamageExpression(str: String) = parseToEither(str, diceFactor)

  private def parseToEither(input: String, factor: Parser[Term]): Either[String, Term] = {
    val pr = parseAll(expr(factor), input)
    if (pr.successful) {
      Right(pr.get)
    } else {
      Left("Failed parse" + pr.toString)
    }
  }
}