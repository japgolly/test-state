package teststate

// *******************************************
// ******************* JVM *******************
// *******************************************
object Platform {

  val Name = "JVM"

  val StackTestSize = 100000

//  val UnitToBoolException = "scala.runtime.BoxedUnit cannot be cast to java.lang.Boolean"
  val UnitToBoolException = "java.lang.ClassCastException"
}