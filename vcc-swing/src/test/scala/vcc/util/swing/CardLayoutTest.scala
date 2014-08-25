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
package vcc.util.swing

import org.uispec4j.{UISpec4J, UISpecTestCase}
import swing.Button
import org.uispec4j.finder.ComponentMatchers
import javax.swing.JButton
import org.uispec4j.assertion.Assertion

class CardLayoutTest extends UISpecTestCase {

  private val cardPanel = new CardPanel()

  override def setUp() {
    super.setUp()

    List("one", "two", "three").foreach(label => cardPanel.addCard(new Button(label), label))
    setAdapter(new SwingComponentWrapperAdapter(cardPanel))
  }

  def testAskCardWithNoAddedCards() {
    val cardPanel = new CardPanel()
    println(cardPanel.displayingCard)
    assertTrue(assertion("No card showed")(cardPanel.displayingCard == None))
  }

  def testShowFirstTab() {
    assertTrue(assertion("Button one is visible")(buttonLabels() == List("one")))
    assertTrue(cardPanel.isShowingCard("one"))
  }

  def testShowSecondTab() {
    cardPanel.showCard("two")
    assertTrue(assertion("Button two is visible")(buttonLabels() == List("two")))
    assertTrue(cardPanel.isShowingCard("two"))
  }

  def testShowThreeTab() {
    cardPanel.showCard("three")
    assertTrue(assertion("Button three is visible")(buttonLabels() == List("three")))
    assertTrue(cardPanel.isShowingCard("three"))
  }

  def testNoChangeOnMissingCard() {
    cardPanel.showCard("not found")
    assertTrue(assertion("Button one is visible")(buttonLabels() == List("one")))
    assertTrue(cardPanel.isShowingCard("one"))
  }

  def buttonLabels(): List[String] = {
    getMainWindow.getSwingComponents(ComponentMatchers.fromClass(classOf[JButton])).toList.map(x => x.asInstanceOf[JButton].getText)
  }

  def assertion(msg: String)(test: => Boolean): Assertion = {
    new Assertion {
      def check() {
        if (!test) throw new AssertionError(msg)
      }
    }
  }

  private implicit class PimpedCardPanel(panel: CardPanel) {
    def isShowingCard(expected: String) = new Assertion {
      def check() = {
        if(panel.displayingCard != Some(expected))
            throw new AssertionError(s"Panel showing card ${panel.displayingCard} expected $expected")
      }
    }

  }

}