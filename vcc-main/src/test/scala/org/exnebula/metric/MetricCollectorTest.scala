/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package org.exnebula.metric

import org.specs2.SpecificationWithJUnit
import org.specs2.matcher.{Matcher, MatchResult}
import vcc.dnd4e.{BootStrap, Configuration}
import vcc.infra.ConfigurationFinder
import vcc.dnd4e.compendium.{CompendiumRepository, Compendium}

class MetricCollectorTest extends SpecificationWithJUnit {
  def is =
    "MetricCollector".title ^
      "Collect screen resolution" ! check("screensize", beMatching( """\d+x\d+""")) ^
      "Have os" ! checkEqual("os.name", System.getProperty("os.name")) ^
      "Have vcc version" ! checkEqual("vcc.version", BootStrap.version.versionString) ^
      "Have java specification" ! checkEqual("java.spec", System.getProperty("java.specification.version")) ^
      "Have total captured creatures" ! check("compendium.captured", beMatching( """\d+""")) ^
      "Have total monsters" ! check("compendium.creatures", beMatching( """\d+""")) ^
      "Have total characters" ! check("compendium.characters", beMatching( """\d+""")) ^
      "Have total traps" ! check("compendium.traps", beMatching( """\d+""")) ^
      end

  private val collected = collectMetrics()

  private def collectMetrics(): Option[Map[String, String]] = {
    if (ConfigurationFinder.locateFile() != null) {
      Configuration.load(ConfigurationFinder.locateFile())
      Compendium.setActiveRepository(new CompendiumRepository(Configuration.compendiumStoreID.value))
      Some(new MetricCollector().collect(Configuration.baseDirectory.value))
    } else {
      println("[Warning] Ignoring this test since we have no configuration file.")
      None
    }
  }

  private def check(key: String, matcher: Matcher[String]): MatchResult[Any] =
    if(collected.isDefined)
      (collected.get must haveKey(key)) and (collected.get(key) must matcher)
    else
      Matcher.success("Ignoring since we have no configuration", null)

  private def checkEqual(key: String, expected: String) = check(key, beEqualTo(expected))
}