package teststate.external

object MyTestState
  extends teststate.Exports
  with teststate.ExtCatsEffect

object CompilationTest {
  import MyTestState._

  val ioDsl = Dsl.io[Unit, Unit, Unit]
}
