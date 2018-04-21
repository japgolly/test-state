package teststate.example.selenium

/**
  * Build a TestState configuration for this project.
  *
  * See `/doc/USAGE.md` for more info.
  */
object MyTestState
  extends teststate.Exports
    with teststate.domzipper.selenium.Exports {

  lazy val CI: Option[String] =
    Option(System.getProperty("CI")).map(_.toLowerCase.trim).filter(_.nonEmpty)

}

