/**
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
//$Id$
package vcc.dnd4e 


import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4,JUnitSuiteRunner}
import vcc.util.swing.XHTMLPaneAgent
import view.{EffectEditor, IconLibrary}

@RunWith(classOf[JUnitSuiteRunner])
class BootstrapTest extends JUnit4(BootstrapSpec)

/**
 * Class to test singleton objects.
 */
object BootstrapSpec extends Specification {
  "IconLibrary must startup" in {
    IconLibrary.MetalD20 mustNot beNull
  }

  "Load XHTMLPaneAgent configured properly" in {
    XHTMLPaneAgent.createInstance(Configuration.dataDirectory)
    XHTMLPaneAgent.isStartupComplete must beTrue
  }

  "Load AutoComplete term" in {
    EffectEditor.dictionary.findSuggestion("ong") must_== Some("ongoing")
    EffectEditor.dictionary.findSuggestion("reg") must_== Some("regenerate")
  }

}