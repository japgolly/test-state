package teststate.example.react

/**
  * Build a TestState configuration for this project.
  *
  * See `/doc/USAGE.md` for more info.
  */
object TestState
  extends teststate.Exports
     with teststate.ExtScalaJsReact
     with teststate.domzipper.sizzle.Exports
