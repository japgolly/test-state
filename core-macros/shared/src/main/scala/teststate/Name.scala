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

  def lazily(n: => Name): Name =
    new Name(() => n.value) // TODO Being a bit lazy here

  trait Implicits {
//    implicit def nameFromString(s: String): Name = Name(s)
    implicit def materializeNameFromString(body: String): Name = macro Name.MacroImpls.name
    implicit def materializeNameFnFromString(body: String): Name.Fn[Any] = macro Name.MacroImpls.nameFn
//    implicit def materializeNameFnFromString[A](body: A)(implicit ev: A => Name): Name.Fn[Any] = macro Name.MacroImpls.nameFn2[A]
    implicit def nameFnFromString[A](a: A)(implicit ev: A => Name): Name.Fn[Any] = Function const ev(a)
  }

  import scala.reflect.macros.blackbox.Context

  final class MacroImpls(val c: Context) {
    import c.universe.{Name => _, _}

    def name(body: c.Expr[String]): c.Expr[Name] =
      c.Expr[Name](q"_root_.teststate.Name($body)")

    def nameFn(body: c.Expr[String]): c.Expr[Name.Fn[Any]] =
      c.Expr[Name.Fn[Any]](q"Function const _root_.teststate.Name($body)")

//    def nameFn2[A](body: c.Expr[A])(ev: c.Expr[A => Name]): c.Expr[Name.Fn[Any]] =
//      c.Expr[Name.Fn[Any]](q"Function const $ev($body)")
  }
}
