package teststate

final class Name(init: () => String) {
  private[this] var thunk = init

  lazy val value: String = {
    val n = thunk()
    thunk = null // dereference
    n
  }
}

object Name {
  type Fn[A] = Option[A] => Name

  def apply(n: => String): Name =
    new Name(() => n)
}
