package teststate.dsl

trait HasErrorString {
  def errorString: String
}

object HasErrorString {
  implicit def formatHasErrorString(e: HasErrorString): String =
    e.errorString
}
