package teststate.external

object MyTestState
  extends teststate.Exports
  with teststate.ExtZio

object CompilationTest {
  import MyTestState._

  val taskDsl = Dsl.task[Unit, Unit, Unit]
}
