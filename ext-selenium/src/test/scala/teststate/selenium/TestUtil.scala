package teststate.selenium

object TestUtil extends japgolly.microlibs.testutil.TestUtil {

  lazy val CI: Option[String] =
    Option(System.getProperty("CI")).map(_.toLowerCase.trim).filter(_.nonEmpty)

}
