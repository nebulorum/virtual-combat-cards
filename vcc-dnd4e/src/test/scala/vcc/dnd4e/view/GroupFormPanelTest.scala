/*
 * Copyright (C) 2013-2014 - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your` option) any later version.
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

import scala.swing._
import vcc.util.swing.MigPanel
import org.uispec4j.{Window, UISpecAdapter, UISpecTestCase}
import org.uispec4j.assertion.Assertion
import scala.swing.event.ValueChanged
import vcc.dnd4e.view.GroupFormPanel.{FormSave, FormValueChanged}
import javax.swing.JLabel

class GroupFormPanelTest extends UISpecTestCase {

  import GroupFormPanelTest._

  private val alice = People("Alice", 28)
  private val bob = People("Bob", 33)
  private val charlie = People("Charlie", 20)

  private var panel: GroupFormPanel[People] = null

  override def setUp() {
    super.setUp()
    val form = new PeopleForm
    panel = new GroupFormPanel[People](form, form, _.asListView)
    setAdapter(new UISpecAdapter() {
      def getMainWindow: Window = {
        val frame = new Frame {
          contents = panel
        }
        new Window(frame.peer)
      }
    })
  }
  
  def testWithEmptyList_showBlankForm() {
    assertBlankFormShown()
    assertThat(not(getBackButton.isEnabled))
    assertThat(not(getCopyButton.isEnabled))
    assertThat(labelMatches("form.entryLabel", "Entry"))
    assertThat(getSaveButton.tooltipEquals("Save Entry"))
    assertThat(getCopyButton.tooltipEquals("Copy Entry"))
    assertThat(getDeleteButton.tooltipEquals("Delete Entry"))
    assertThat(getBackButton.tooltipEquals("Show all Entries"))
  }

  def testWithContent_mustShowList() {
    panel.setContent(Seq(alice))
    assertThat(getGroupList.contains(alice.asListView))
    assertThat(labelMatches("group.entriesLabel", "Entries"))
    assertThat(getNewButton.tooltipEquals("Create new Entry"))
  }

  def testCustomizingPanel_shouldChangeMessages() {
    panel.setContent(Seq(alice))
    panel.setHeaderLabels("Person", "Persons")
    assertThat(labelMatches("group.entriesLabel", "Persons"))
    assertThat(getNewButton.tooltipEquals("Create new Person"))
  }

  def testCustomizingPanel_shouldChangeFormMessages() {
    panel.setHeaderLabels("Person", "Persons")
    assertThat(labelMatches("form.entryLabel", "Person"))
    assertThat(getSaveButton.tooltipEquals("Save Person"))
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

    getBackButton.click()

    assertThat(getGroupList.isVisible)
    assertThat(getGroupList.selectionIsEmpty())
  }

  def testByDefaultBlankFromHasSaveDisabled() {
    assertThat(getSaveButton.isVisible)
    assertFalse(getSaveButton.isEnabled)
    getFormName.setText("")
  }

  def testSettingValidFields_shouldEnableSave() {
    getFormName.setText("David")
    getFormAge.setText("3")
    assertThat(getSaveButton.isVisible)
    assertThat("save should be enabled", getSaveButton.isEnabled)
  }

  def testSettingInvalidFields_shouldKeepSaveDisabled() {
    getFormName.setText("David")
    getFormAge.setText("a")
    assertThat(getSaveButton.isVisible)
    assertFalse(getSaveButton.isEnabled)
  }

  def testWhenSelectingEntry_saveShouldBeEnabled() {
    panel.setContent(Seq(alice, bob))
    getGroupList.selectIndices(0)
    assertThat(getSaveButton.isVisible)
    assertThat(getSaveButton.isEnabled)
  }

  def testWhenNewFormAndDataInvalidLeaveSaveDisabled() {
    panel.setContent(Seq(alice, bob))
    getGroupList.selectIndices(0)
    getFormAge.setText("NaN")
    assertFalse("Save should be disabled", getSaveButton.isEnabled)
  }

  def testWhenSaveIsValidClickingSave_shouldSaveEntryOnListAndGoBack() {
    panel.setContent(Seq(alice, bob, charlie))

    getGroupList.selectIndex(1)
    getFormAge.setText("10")
    getFormName.setText("Bobby")
    getSaveButton.click()
    assertThat("Saved data not matching",
      panelContentMatches(panel, Seq(alice, People("Bobby", 10), charlie)))
    assertThat(getGroupList.isVisible)
  }

  def testWhenFormIsValidTriggeringSaveEvent_shouldSaveContentAndStayInForm() {
    panel.setContent(Seq(alice))
    getGroupList.selectIndex(0)
    getFormName.setText("Alister")
    getMainWindow.getButton("form.innerButton").click()
    assertThat(panelContentMatches(panel, Seq(People("Alister", alice.age))))
    assertThat(getFormName.isVisible)
  }

  def testWhenFormIsInvalidTriggeringSaveEvent_shouldNotSaveContentAndStayInForm() {
    panel.setContent(Seq(alice))
    getGroupList.selectIndex(0)
    getFormAge.setText("Alister")
    getMainWindow.getButton("form.innerButton").click()
    assertThat(panelContentMatches(panel, Seq(alice)))
    assertThat(getFormName.isVisible)
  }

  def testWhenClickingNewButton_shouldShowBlankForm() {
    panel.setContent(Seq(alice))
    getGroupList.selectIndex(0)
    getBackButton.click()
    getNewButton.click()
    assertBlankFormShown()
  }

  def testAfterClickNewAndSavingNewForm_shouldHaveNewEntryInTopOfList() {
    panel.setContent(Seq(bob))
    getNewButton.click()
    fillInFormFor(alice)
    assertThat(panelContentMatches(panel, Seq(alice, bob)))
  }

  def testGoingBackAndForthBetweenFormAndList_shouldRespectNewPolice() {
    panel.setContent(Seq(bob))
    getGroupList.selectIndex(0)
    getBackButton.click()
    getNewButton.click()
    fillInFormFor(alice)
    assertThat(panelContentMatches(panel, Seq(alice, bob)))
  }

  def testGoingIntoFormAndClickingDelete_shouldRemoveEntryAndGoBack() {
    panel.setContent(Seq(alice, bob, charlie))
    getGroupList.selectIndex(1)
    getDeleteButton.click()
    assertThat(panelContentMatches(panel, Seq(alice, charlie)))
    assertThat(getGroupList.isVisible)
  }

  def testNewFormShouldNotHaveDeleteEnabled() {
    panel.setContent(Seq(alice))
    getNewButton.click()
    assertThat(not(getDeleteButton.isEnabled))
  }

  def testDeletingLastInEntryListShouldWork() {
    panel.setContent(Seq(alice, bob, charlie))
    getGroupList.selectIndex(2)
    getDeleteButton.click()
    assertThat(panelContentMatches(panel, Seq(alice, bob)))
    assertThat(getGroupList.isVisible)
  }

  def testAfterDeletingAllEntries_shouldGoToBlankForm() {
    panel.setContent(Seq(alice))
    getGroupList.selectIndex(0)
    getDeleteButton.click()
    assertThat(panelContentMatches(panel, Seq()))
    assertBlankFormShown()
    assertThat(not(getBackButton.isEnabled))
  }

  def testDeletingCopiedInstances_shouldRemoveOnlyOneCopy() {
    panel.setContent(Seq(alice, bob, bob, charlie))
    getGroupList.selectIndex(1)
    getDeleteButton.click()
    assertThat(panelContentMatches(panel, Seq(alice, bob, charlie)))
  }

  def testOnceSelectedSavedEntryCopying_shouldResultInNewEntryAfterFirst() {
    panel.setContent(Seq(alice, bob, charlie))
    getGroupList.selectIndex(1)
    getCopyButton.click()
    getFormName.setText("Bobby")
    getSaveButton.click()
    assertThat(panelContentMatches(panel, Seq(alice, bob.copy(name = "Bobby"), bob, charlie)))
  }

  private def getFormName = getMainWindow.getInputTextBox("form.name")

  private def getFormAge = getMainWindow.getInputTextBox("form.age")

  private def getSaveButton = getMainWindow.getButton("form.save")

  private def getBackButton = getMainWindow.getButton("form.back")

  private def getGroupList = getMainWindow.getListBox("group.list")

  private def getNewButton = getMainWindow.getButton("group.newButton")

  private def getDeleteButton = getMainWindow.getButton("form.delete")

  private def getCopyButton = getMainWindow.getButton("form.copy")

  private def fillInFormFor(entry: People) {
    getFormName.setText(entry.name)
    getFormAge.setText(entry.age.toString)
    getSaveButton.click()
  }

  private def assertBlankFormShown() {
    assertThat(getFormName.textIsEmpty)
    assertThat(getFormAge.textIsEmpty)
    assertThat(getFormName.isVisible)
    assertThat(getFormAge.isVisible)
    assertThat(not(getSaveButton.isEnabled))
    assertThat(not(getCopyButton.isEnabled))
  }

  private def panelContentMatches[T](panel: GroupFormPanel[T], expectedContent: Seq[T]): Assertion = {
    new Assertion {
      def check() {
        if (panel.getContent != expectedContent)
          throw new AssertionError(s"Panel content: ${panel.getContent} does not match expected content: $expectedContent")
      }
    }
  }

  private def labelMatches(labelName: String, expectedText: String) = {
    new Assertion {
      def check() {
        val label = getMainWindow.findSwingComponent(classOf[JLabel], labelName)
        if (label == null)
          throw new AssertionError(s"Could not find label with name: $labelName")

        val labelText = label.getText
        if (labelText != expectedText)
          throw new AssertionError(s"Label $labelName text does not match expected. Found: $labelText Expected: $expectedText")
      }
    }
  }
}

object GroupFormPanelTest {

  case class People(name: String, age: Int) {
    def asListView: String  =
      s"""<html><body><strong>$name</strong><br/>&nbsp;$age</body></html>"""
  }


  class PeopleForm extends MigPanel("fillx, ins panel", "[]rel[grow]", "[][]") with GroupFormPanel.Presenter[People] {
    val nameField = new TextField()
    nameField.name = "form.name"
    val ageField = new TextField()
    ageField.name = "form.age"
    val actionButton = new Button(Action("Save") {
      publish(FormSave(this))
    })
    actionButton.name = "form.innerButton"

    init()

    private def init() {
      add(new Label("Name"))
      add(nameField, "wrap, grow")
      add(new Label("Age"))
      add(ageField, "wrap, grow")
      add(actionButton, "span 2, wrap")
      add(new ScrollPane(new TextArea("Sample text not part of form")), "span 2, growx, growy, push")
      listenTo(ageField)
      reactions += {
        case ValueChanged(this.ageField) =>
          publish(FormValueChanged(this, valid = isAllDigits(ageField.text)))
      }
    }

    def setEntry(entry: People) {
      nameField.text = entry.name
      ageField.text = entry.age.toString
    }

    def getEntry = People(nameField.text, ageField.text.toInt)

    def isValid: Boolean = isAllDigits(ageField.text)

    def clear() {
      ageField.text = ""
      nameField.text = ""
    }
  }

  private def isAllDigits(x: String) = (x forall Character.isDigit) && (x != "")

}

object GroupFormExample {

  import GroupFormPanelTest._

  val frame = new MainFrame {
    private val panel: GroupFormPanel[People] = new GroupFormPanel[People](new PeopleForm, _.asListView)
    contents = panel
    panel.setHeaderLabels("Person", "Persons")
    panel.setContent(Seq(People("Alice", 19), People("Bob", 33), People("charlie", 25)))
  }

  def main(args: Array[String]) {
    frame.visible = true
  }
}