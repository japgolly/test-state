package teststate.external

object TestState
  extends teststate.Exports
  with teststate.ExtZio

object CompilationTest {
  import TestState._

  val taskDsl = Dsl.task[Unit, Unit, Unit]
}
