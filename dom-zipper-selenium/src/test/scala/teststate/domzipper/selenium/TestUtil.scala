package teststate.domzipper.selenium

import java.io.File

object SeleniumTestUtil {

  lazy val baseDirectory: File = {
    val p = "sbt.baseDirectory"
    val d = Option(System.getProperty(p)).getOrElse(sys.error(s"Property [$p] isn't specified."))
    val f = new File(d)
    require(f.exists() && f.isDirectory(), s"Directory not found: ${f.getAbsolutePath}")
    f
  }

  def fromBaseDirectory(suffix: String): File = {
    val f = baseDirectory
    new File(s"${f.getAbsolutePath}/$suffix")
  }

  def testResource(path: String): File =
    fromBaseDirectory(s"src/test/resources/$path")

}
