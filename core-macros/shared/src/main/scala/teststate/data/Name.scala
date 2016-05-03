package teststate.data

import acyclic.file
import scala.reflect.macros.blackbox

abstract class Name {
  def value: String
  def map(f: String => String): Name
}

object Name {

  final class Now(override val value: String) extends Name {
    override def map(f: String => String): Name =
      now(f(value))
  }

  final class Later(init: () => String) extends Name {
    private[this] var thunk = init

    override lazy val value: String = {
      val n = thunk()
      thunk = null // dereference
      n
    }

    override def map(f: String => String): Name =
      Name(f(value))
  }

  def now(value: String): Now =
    new Now(value)

  def apply(n: => String): Name =
    new Later(() => n)

  def lazily(n: => Name): Name =
    new Later(() => n.value)

  trait Implicits {
    implicit def materializeNameFromString(body: String): Name =
      macro Name.MacroImpls.name

    implicit def materializeNameFnFromString(body: String): NameFn[Any] =
      macro Name.MacroImpls.nameFn

    implicit def nameFnFromString[A](a: A)(implicit ev: A => Name): NameFn[Any] =
      NameFn const ev(a)
  }

  object Implicits extends Implicits

  final class MacroImpls(val c: blackbox.Context) {
    import c.universe.{Name => _, _}

    def name(body: c.Expr[String]): c.Expr[Name] =
      body match {
        case Expr(Literal(Constant(s: String))) =>
          c.Expr[Name](q"_root_.teststate.data.Name.now($s)")
        case _ =>
          c.Expr[Name](q"_root_.teststate.data.Name($body)")
      }

    def nameFn(body: c.Expr[String]): c.Expr[NameFn[Any]] =
      c.Expr[NameFn[Any]](q"_root_.teststate.data.NameFn.const($body)")
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

  def mapContextFree(n: Name): NameFn[A] =
    NameFn {
      case s@Some(_) => fn(s)
      case None      => n
    }
}

object NameFn {
  def const(n: Name): NameFn[Any] =
    NameFn(_ => n)
}
