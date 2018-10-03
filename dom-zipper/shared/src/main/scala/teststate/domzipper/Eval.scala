package teststate.domzipper

final case class Eval[A](eval: () => A) //extends AnyVal {

object Eval {

  def value[A](a: A): Eval[A] =
    apply(() => a)

  trait Strategy {
    def apply[A](a: => A): Eval[A]
  }

  object ByName extends Strategy {
    override def apply[A](a: => A): Eval[A] =
      Eval(() => a)
  }

  object ByNeed extends Strategy {
    override def apply[A](a: => A): Eval[A] = {
      lazy val l = a
      Eval(() => l)
    }
  }

}