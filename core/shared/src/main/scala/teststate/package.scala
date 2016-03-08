import teststate.data.Name

package object teststate extends Name.Implicits {

  trait HasErrorString {
    def errorString: String
  }

  implicit def formatHasErrorString(e: HasErrorString): String =
    e.errorString

  // ===================================================================================================================

  @inline private[teststate] def vector1[A](a: A): Vector[A] =
    Vector.empty[A] :+ a
}
