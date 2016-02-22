package teststate

final class Name(init: () => String) {
  private[this] var thunk = init

  lazy val value: String = {
    val n = thunk()
    thunk = null // dereference
    n
  }

  def map(f: String => String): Name =
    Name(f(value))
}

object Name {

  def apply(n: => String): Name =
    new Name(() => n)

  def lazily(n: => Name): Name =
    new Name(() => n.value) // TODO Being a bit lazy here

  // TODO Is this needed? Just put in comp objects, no?
  trait Implicits {
//    implicit def nameFromString(s: String): Name = Name(s)
    implicit def materializeNameFromString(body: String): Name = macro Name.MacroImpls.name
    implicit def materializeNameFnFromString(body: String): NameFn[Any] = macro Name.MacroImpls.nameFn
//    implicit def materializeNameFnFromString[A](body: A)(implicit ev: A => Name): NameFn[Any] = macro Name.MacroImpls.nameFn2[A]
    implicit def nameFnFromString[A](a: A)(implicit ev: A => Name): NameFn[Any] = NameFn const ev(a)
//    implicit def nameFnFromFn[A](f: Option[A] => Name): NameFn[A] = Fn(f)
  }

  import scala.reflect.macros.blackbox.Context

  final class MacroImpls(val c: Context) {
    import c.universe.{Name => _, _}

    def name(body: c.Expr[String]): c.Expr[Name] =
      c.Expr[Name](q"_root_.teststate.Name($body)")

    def nameFn(body: c.Expr[String]): c.Expr[NameFn[Any]] =
      c.Expr[NameFn[Any]](q"_root_.teststate.NameFn.const($body)")

//    def nameFn2[A](body: c.Expr[A])(ev: c.Expr[A => Name]): c.Expr[NameFn[Any]] =
//      c.Expr[NameFn[Any]](q"Function const $ev($body)")
  }
}

// =====================================================================================================================

final case class NameFn[-A](fn: Option[A] => Name) extends AnyVal {
  @inline def apply(i: Option[A]) =
    fn(i)

  def map(f: Name => Name): NameFn[A] =
    NameFn(f compose fn)

  def cmap[B](f: B => A): NameFn[B] =
    NameFn(ob => apply(ob map f))

  def comap[B](f: B => Option[A]): NameFn[B] =
    NameFn(ob => apply(ob flatMap f))

  //    def pmapFnE[A, B](fn: Fn[A])(f: B => Either[Nothing, A]): Fn[B] =
  //      pmapFn(fn)(f(_).right.toOption) // TODO Use custom toOption after moving EitherExt into own package
}

object NameFn {
  def const(n: Name): NameFn[Any] =
    NameFn(_ => n)
}
