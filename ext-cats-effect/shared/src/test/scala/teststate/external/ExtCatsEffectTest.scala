package teststate.external

object TestState
  extends teststate.Exports
  with teststate.ExtCatsEffect

object CompilationTest {
  import TestState._

  val ioDsl = Dsl.io[Unit, Unit, Unit]
}
