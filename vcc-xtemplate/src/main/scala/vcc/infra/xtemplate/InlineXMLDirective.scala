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
package vcc.infra.xtemplate

import scala.xml.{NodeSeq, Node}

/**
 * Directive that will render the TemplateDataSource getInlineXML element if present or nothing that inline is not defined.
 */
object InlineXMLDirective extends TemplateDirective[String]("inline", true) {
  def render(ds: TemplateDataSource, node: TemplateNode[String]): NodeSeq = ds.templateInlineXML(node.arguments)

  protected def processArguments(node: Node, engine: TemplateEngine, template: Template, child: NodeSeq): String = validateSingleArgument("id", node)
}