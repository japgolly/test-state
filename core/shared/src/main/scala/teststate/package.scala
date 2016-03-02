package object teststate extends teststate.Name.Implicits {

  implicit def sadfhasdlfkj[F[_], R, O, S, E](b: Dsl.ActionB[F, R, O, S, E]) = b.noStateUpdate

  trait ~~>[F[_], G[_]] {
    def apply[A](fa: => F[A]): G[A]
  }

  type Id[A] = A

  trait HasErrorString {
    def errorString: String
  }

  implicit def formatHasErrorString(e: HasErrorString): String =
    e.errorString

  // ===================================================================================================================

  @inline private[teststate] def vector1[A](a: A): Vector[A] =
    Vector.empty[A] :+ a

  private[teststate] def wrapWithCond[A, B](c: A => Boolean, f: A => Option[B]): A => Option[B] =
    a => if (c(a)) f(a) else None

  private[teststate] def wrapWithCond2[A, B, C](c: A => Boolean, f: A => TriResult[B, C]): A => TriResult[B, C] =
    a => if (c(a)) f(a) else Skipped
}
