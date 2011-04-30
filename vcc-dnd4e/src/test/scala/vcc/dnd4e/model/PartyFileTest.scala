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
//$Id$
package vcc.dnd4e.model

import org.specs.SpecificationWithJUnit
import org.specs.mock.Mockito
import vcc.infra.datastore.naming.EntityID
import vcc.dnd4e.domain.tracker.common.CombatantID
import java.io._

class PartyFileTest extends SpecificationWithJUnit with Mockito {

  private val xmlPrefix = "<?xml version='1.0' ?>\n"
  private val entId1 = EntityID.fromName("test:1")
  private val entId2 = EntityID.fromName("test:2")

  "PartyFile" should {
    "gracefully return on bad stream" in {
      val mStream = mock[InputStream]
      mStream.read(any) throws (new IOException("broken stream"))

      PartyFile.loadFromStream(mStream) must_== (Nil, false)
    }

    "gracefully return on invalid file" in {
      val is = string2Stream("not a xml file")
      PartyFile.loadFromStream(is) must_== (Nil, false)
    }

    "gracefully return on old party file" in {
      val is = string2Stream(xmlPrefix + "<party><some/></party>")
      PartyFile.loadFromStream(is) must_== (Nil, false)
    }

    "return (Nil,true) on empty file" in {
      val is = string2Stream(xmlPrefix + "<party version='1.0'></party>")
      PartyFile.loadFromStream(is) must_== (Nil, true)
    }

    "return list of party of two" in {
      val is = makePartyFile(
        "<combatant eid='" + entId1.asStorageString + "' />",
        "<combatant eid='" + entId2.asStorageString + "' />")
      PartyFile.loadFromStream(is) must_== (List(PartyMember(null, null, entId1), PartyMember(null, null, entId2)), true)
    }

    "return list of party of two one with alias the other with id" in {
      val is = makePartyFile(
        "<combatant eid='" + entId1.asStorageString + "' id='A' />",
        "<combatant eid='" + entId2.asStorageString + "' alias='some other name'/>")
      PartyFile.loadFromStream(is) must_== (List(PartyMember(CombatantID("A"), null, entId1), PartyMember(null, "some other name", entId2)), true)
    }

    "return list of party one combatant and another broken" in {
      val is = makePartyFile(
        "<combatant eid='" + entId1.asStorageString + "' />",
        "<combatant />")
      PartyFile.loadFromStream(is) must_== (List(PartyMember(null, null, entId1)), false)
    }

    "return list of party one combatant and another failed" in {
      val is = makePartyFile(
        "<combatant eid='" + entId1.asStorageString + "' />",
        "<trash />")
      PartyFile.loadFromStream(is) must_== (List(PartyMember(null, null, entId1)), false)
    }

    "load what it saves" in {
      val file = File.createTempFile("test", "xml")
      val partyList = List(
        PartyMember(CombatantID("A"), null, entId1),
        PartyMember(null, "dude", entId2),
        PartyMember(null, null, entId2))
      PartyFile.saveToFile(file, partyList)
      PartyFile.loadFromStream(new FileInputStream(file)) must_== (partyList, true)
      file.delete()
    }
  }

  def string2Stream(s: String): InputStream = new ByteArrayInputStream(s.getBytes("UTF-8"))

  def makePartyFile(members: String*): InputStream = {
    val sb = new StringBuilder

    sb.append(xmlPrefix)
    sb.append("<party version='1.0' >\n")
    for (member <- members) {
      sb.append(member)
      sb.append("\n")
    }
    sb.append("</party>\n")
    string2Stream(sb.toString)
  }
}