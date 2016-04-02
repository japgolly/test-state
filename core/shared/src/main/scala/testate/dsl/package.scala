package testate

import acyclic.file

package object dsl {

  trait HasErrorString {
    def errorString: String
  }

  implicit def formatHasErrorString(e: HasErrorString): String =
    e.errorString
}
