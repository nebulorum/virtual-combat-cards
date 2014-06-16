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
package vcc.updater

import java.net.URL
import org.xml.sax.InputSource
import scala.xml.XML
import scala.swing.Frame
import java.awt.Image
import java.io.{InputStream, File}
import vcc.infra.util.RemoteFile
import vcc.util.XMLHelper
import vcc.util.swing.{SwingHelper, MultiPanel}

/**
 * Utility functions to help with update manager
 */
object UpdateManager {

  /**
   * This is based on Drupal project versioning, we assume all version field are Int
   * except extra (which is a nullable string)
   */
  case class Version(major: Int, minor: Int, patch: Int, extra: String) extends Ordered[Version] {
    def compare(that: Version): Int = {
      var dif = this.major - that.major
      if (dif == 0) dif = this.minor - that.minor
      if (dif == 0) dif = this.patch - that.patch
      if (dif == 0)
        if (this.extra != null && that.extra != null) dif = this.extra.compare(that.extra)
        else if (this.extra != null) dif = -1
        else if (that.extra != null) dif = 1
      dif
    }

    def versionString: String = major + "." + minor + "." + patch + (if (extra != null) "-" + extra else "")

    def isPatch(other: Version): Boolean = {
      (this.major == other.major) && (this.minor == other.minor) && this.patch > other.patch
    }

    def isEligibleUpgradeFromVersion(fromVersion: Version): Boolean = {
      this > fromVersion && ((this.major == fromVersion.major && this.minor == fromVersion.minor) || this.patch == 0)
    }
  }

  val NotFoundVersion = Version(0, 0, 0, "NotFound")

  object Version {
    private val fullVersion = """(\d+)\.(\d+)\.(\d+)(\-\w+)?""".r
    private val partialVersion = """(\d+)\.(\d+)(\-\w+)?""".r

    /**
     * Load a version from a string
     * @return Version if format is correct, null otherwise
     */
    def fromString(str: String): Version = {
      str match {
        case null => null
        case fullVersion(major, minor, patch, qualifier) =>
          val modQualifier = if (qualifier != null) qualifier.substring(1) else null
          Version(major.toInt, minor.toInt, patch.toInt, modQualifier)
        case partialVersion(major, minor, qualifier) =>
          val modQualifier = if (qualifier != null) qualifier.substring(1) else null
          Version(major.toInt, minor.toInt, 0, modQualifier)
        case _ => null
      }
    }

    /**
     * Loads a version file and extract the version from it
     */
    def fromVersionFileFromStream(resource: InputStream): Version = {
      try {
        val versionXml = XML.load(new InputSource(resource))
        if (versionXml.label == "version") this.fromString(versionXml.text) else NotFoundVersion
      } catch {
        case _: Exception => NotFoundVersion
      }
    }
  }

  /**
   * Case class containing basic information on a release, based on Drupal.org
   * release-history format
   * @param version A version string, e.g. 0.99.1
   * @param download URL to download file
   * @param md5 MD4 signature for file
   * @param info URL to release note
   */
  case class Release(version: Version, download: URL, md5: String, info: URL)

  /**
   * Collect version information from a stream, which should point to a XML file. This
   * can be a URL on a remote site. Only published versions should be returned.
   * @param stream The InputSource for the XML file
   * @return A valid current Version or null if something went wrong.
   */
  def checkAvailableVersions(stream: InputSource): Seq[Release] = {
    val release = XML.load(stream)
    val useUnpublished = System.getProperty("vcc.update.unpublished") != null

    if (release.scope.uri != "http://purl.org/dc/elements/1.1/")
      throw new RuntimeException("Not a drupal project file")
    val releases: Seq[Release] = (release \\ "release").map {
      release =>
        val major = XMLHelper.nodeSeq2Int(release \ "version_major", 0)
        val minor = XMLHelper.nodeSeq2Int(release \ "version_minor", 0)
        val patch = XMLHelper.nodeSeq2Int(release \ "version_patch", 0)
        val extra = XMLHelper.nodeSeq2String(release \ "version_extra", null)
        val download = XMLHelper.nodeSeq2String(release \ "download_link", null)
        val md5 = XMLHelper.nodeSeq2String(release \ "mdhash", null)
        val info = XMLHelper.nodeSeq2String(release \ "release_link", null)

        if ((!useUnpublished && XMLHelper.nodeSeq2String(release \ "status") != "published") || download == null) null
        else Release(
          Version(major, minor, patch, extra),
          if (download != null) new URL(download) else null,
          md5,
          if (info != null) new URL(info) else null
        )
    }.filter(r => r != null)
    assert(releases != null)
    releases
  }

  /**
   * Get version information based on an URL
   * @param url URL to the site that contains a Drupal.org release-history XML file
   * @return A valid current Version or null if something went wrong.
   */
  def checkAvailableVersions(url: URL): Seq[Release] = {
    val stream = url.openStream()
    checkAvailableVersions(new InputSource(stream))
  }

  /**
   * @return ( possible release, hasMore ) hasMore indicates that some newer versions where skipped because of full
   *         upgrade policy.
   */
  protected def scanForVersions(currentVersion: Version, is: InputStream): (List[(Symbol, Release)], Boolean) = {
    val releases = checkAvailableVersions(new InputSource(is))

    val possible = releases.filter(r => {
      r.version > currentVersion
    }).map({
      rel =>
        if (rel.version.isEligibleUpgradeFromVersion(currentVersion)) {
          if (rel.version.extra != null) ('RC, rel)
          else if (rel.version.isPatch(currentVersion)) ('PATCH, rel)
          else ('UPGRADE, rel)
        } else {
          ('NOTALLOWED, rel)
        }
    }).toList
    (possible.filter(x => x._1 != 'NOTALLOWED), possible.exists(x => x._1 == 'NOTALLOWED))
  }


  private def getRemoteFile(url: URL) = new RemoteFile(new File(getInstallDirectory, "vcc.release"), url)

  /**
   * Fetch available version then allow user to download chosen version
   * and leave the version ready to update on next launch.
   * @param url URL to fetch available versions
   * @param currentVersion Version VCC is running
   * @param dialogIcon Icon for the dialog frame
   * @param age Maximum age of local cached version of release file (0 to fetch)
   * @return Success or failure
   */
  def runUpgradeProcess(url: URL, currentVersion: Version, dialogIcon: Image, age: Long) {

    import vcc.updater.ReleaseSelectPanel

    def checkFileMD5Sum(file: File, md5sum: String): Boolean = {
      val chkSum = PackageUtil.fileMD5Sum(file)
      md5sum.toLowerCase == chkSum.toLowerCase
    }
    val umd = new Frame() with MultiPanel {
      title = "Update Virtual Combat Cards"
      minimumSize = new java.awt.Dimension(300, 200)
      iconImage = dialogIcon
    }
    umd.visible = true

    umd.showMessage(wait = false, "Checking for a new version...")

    val releaseFile = getRemoteFile(url)

    val is = releaseFile.fetchIfOlder(age)
    if (is != null) {
      val (releases, hasMore) = scanForVersions(currentVersion, is)
      if (releases.length > 0) {
        val releaseOpt = umd.customPanel(new ReleaseSelectPanel(releases, hasMore))

        if (releaseOpt.isDefined) {
          val release = releaseOpt.get

          val dfile = umd.customPanel(new DownloadPanel(release.download, java.io.File.createTempFile("vcc", ".zip")))
          if (dfile.isDefined) {
            umd.showMessage(wait = false, "Checking and unpacking downloaded file...")
            // We have the file
            if (checkFileMD5Sum(dfile.get, release.md5)) {
              PackageUtil.extractFilesFromZip(dfile.get, getInstallDirectory)
              umd.showMessage(wait = true, "<html><body>Download and extraction completed successfully.<p>To update Virtual Combat Cards, exit and restart it.</body></html>")
            } else {
              umd.showMessage(wait = true, "<html><body>Downloaded file seems to be corrupted. <p> Download and extract manually or report a bug on the site</body></html>")
              SwingHelper.openDesktopBrowser(release.info)
            }
          } else {
            umd.showMessage(wait = true, "Download failed or cancelled.")
          }
        } else {
          // No version selected
        }
      } else {
        umd.showMessage(wait = true, "Your version is up to date.")
      }
    } else {
      umd.showMessage(wait = true, "<html><body>Failed to download Releases history from:<br>" + url + "</body></html>")
    }
    umd.dispose()
  }

  /**
   * Returns a File which points to the directory in which VCC Binary installation is
   * located.
   * @return File The VCC install directory
   */
  def getInstallDirectory: File = new File(System.getProperty("vcc.install", System.getProperty("user.dir", ".")))

  /**
   * Simple check routine to verify if there are upgrades to a version.
   * @param url Release file URL
   * @param currentVersion Current version of VCC
   * @param age Maximum age of the local cached file
   * @return True if there are upgrades available
   */
  def checkForUpgrade(url: URL, currentVersion: Version, age: Long): Boolean = {
    val releaseFile = getRemoteFile(url)

    val is = releaseFile.fetchIfOlder(age)
    if (is != null) {
      val (releases, _) = scanForVersions(currentVersion, is)
      !releases.isEmpty
    } else false
  }
}