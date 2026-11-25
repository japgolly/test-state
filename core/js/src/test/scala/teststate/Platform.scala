package teststate

import scala.concurrent.ExecutionContext

// ******************************************
// ******************* JS *******************
// ******************************************
object Platform {

  val Name = "JS"

  val StackTestSize = 10 //24

  implicit def globalExecutionContext: ExecutionContext =
    scala.scalajs.concurrent.QueueExecutionContext.timeouts()
}
