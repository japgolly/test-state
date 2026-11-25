package teststate

import scala.concurrent.ExecutionContext

// *******************************************
// ******************* JVM *******************
// *******************************************
object Platform {

  val Name = "JVM"

  val StackTestSize = 100000

  implicit def globalExecutionContext: ExecutionContext =
    ExecutionContext.Implicits.global
}
