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
package vcc.infra.diskcache

import java.io.File
import vcc.util.DirectoryIterator

trait DiskCacheBuilder[T] {
  def loadFromFile(file: File): T
}

trait DiskCacheable {
  def saveToCache(file: File): Boolean

  def getCacheFileName: String = System.currentTimeMillis.toString + this.hashCode.toString
}

class DiskCache[T <: DiskCacheable](val dir: File, builder: DiskCacheBuilder[T]) {
  def save(obj: T): Boolean = {
    try {
      val file = new File(dir, obj.getCacheFileName)
      if (!file.getParentFile.exists()) {
        file.getParentFile.mkdirs()
      }
      obj.saveToCache(file)
    } catch {
      case e: Exception => false
    }
  }

  def loadAll(): Seq[T] = {
    val dirIter = new DirectoryIterator(dir, false)

    val l = for (file <- dirIter if (file.isFile)) yield {
      builder.loadFromFile(file)
    }
    l.filter(x => x != null).toList
  }

  def clear() {
    val dirIter = new vcc.util.DirectoryIterator(dir, false)
    dirIter.foreach(file => if (file.isFile) file.delete())
  }
}

