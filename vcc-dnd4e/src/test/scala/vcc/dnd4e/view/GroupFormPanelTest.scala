/*
 * Copyright (C) 2013-2013 - Thomas Santana <tms@exnebula.org>
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

import scala.swing.{Frame, Label, TextField, MainFrame}
import vcc.util.swing.MigPanel
import org.uispec4j.{Window, UISpecAdapter, UISpecTestCase}

class GroupFormPanelTest extends UISpecTestCase {

  import GroupFormPanelTest._

  private val alice = People("Alice", 28)
  private val bob = People("Bob", 33)

  private var panel: GroupFormPanel[People] = null

  override def setUp() {
    super.setUp()
    panel = new GroupFormPanel[People](new PeopleForm)
    setAdapter(new UISpecAdapter() {
      def getMainWindow: Window = {
        val frame = new Frame {
          contents = panel
        }
        new Window(frame.peer)
      }
    })
  }

  def testWithEmptyList_showForm() {
    assertThat(getFormName.isVisible)
    assertThat(getFormAge.isVisible)
  }

  def testWithContent_mustShowList() {
    panel.setContent(Seq(alice))
    assertThat(getGroupList.contains(alice.toString))
  }

  def testWithContent_clickOnListGoesToFrom() {
    panel.setContent(Seq(alice, bob))
    getGroupList.selectIndices(1)
    assertThat(getFormName.isVisible)
    assertThat(getFormAge.isVisible)
    assertThat(getFormName.textEquals(bob.name))
    assertThat(getFormAge.textEquals(bob.age.toString))
  }

  def testGoToFormThenClickBack_shouldReturnToGroupList() {
    panel.setContent(Seq(alice, bob))
    getGroupList.selectIndices(0)

    getMainWindow.getButton("form.back").click()

    assertThat(getGroupList.isVisible)
    assertThat(getGroupList.selectionIsEmpty())
  }

  private def getFormName = getMainWindow.getInputTextBox("form.name")
  private def getFormAge = getMainWindow.getInputTextBox("form.age")
  private def getGroupList = getMainWindow.getListBox("group.list")
}

object GroupFormPanelTest {

  case class People(name: String, age: Int)

  class PeopleForm extends MigPanel("debug,fill") with GroupFormPanel.Form[People] {
    val nameField = new TextField()
    nameField.name = "form.name"
    val ageField = new TextField()
    ageField.name = "form.age"

    add(new Label("Name"))
    add(nameField, "wrap, growx")
    add(new Label("Age"))
    add(ageField, "wrap, growx")
    listenTo(ageField)
    reactions += {
      case scala.swing.event.ValueChanged(this.ageField) =>
        println("Age change: " + ageField.text)
    }

    def setEntry(entry: People) {
      nameField.text = entry.name
      ageField.text = entry.age.toString
    }
  }

}

object GroupFormExample {

  import GroupFormPanelTest._


  val frame = new MainFrame {
    private val panel: GroupFormPanel[People] = new GroupFormPanel[People](new PeopleForm)
    contents = panel
    panel.setContent(Seq(People("Alice", 19), People("Bob", 33), People("charlie", 25)))
//    contents = new PeopleForm
  }

  def main(args: Array[String]) {
    frame.visible = true
  }
}