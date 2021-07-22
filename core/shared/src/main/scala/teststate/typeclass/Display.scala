package teststate.typeclass

import teststate.data.{Name, NameFn}

final class Display[A](private val display: A => String) extends AnyVal {
  def apply(a: A): String =
    display(a)

  def cmap[B](f: B => A): Display[B] =
    Display(b => display(f(b)))

  def map(f: String => String): Display[A] =
    Display(a => f(display(a)))

  def indent: Display[A] =
    indent("  ")

  def indent(i: String): Display[A] =
    map(i + _.replace("\n", "\n" + i))

  def mkString[C[X] <: IterableOnce[X]](start: String, mid: String, end: String): Display[C[A]] =
    Display(_.iterator.map(display).mkString(start, mid, end))

  def coll[C[X] <: IterableOnce[X]]: Display[C[A]] =
    mkString("[", ", ", "]")
}

object Display {
  final class Ops[A](private val a: A)(implicit d: Display[A]) {
    def display: String =
      d(a)
  }

  def apply[A](f: A => String): Display[A] =
    new Display(f)

  def by[A, B](f: A => B)(implicit d: Display[B]): Display[A] =
    new Display(d.display compose f)

  def byToString[A]: Display[A] =
    Display(_.toString)

  def build[A](f: (StringBuilder, A) => Unit): Display[A] =
    Display { a =>
      val sb = new StringBuilder
      f(sb, a)
      sb.result()
    }

  // Putting this into a low-priority implicits trait overrides implicit instances in user-defined companion objects. :(
  implicit def testStateDisplayByToString[A]: Display[A] =
    Display(_.toString)

  def escapeChar(sb: StringBuilder, other: Char)(c: Char): Unit =
    c match {
      case '\b'    => sb append '\\'; sb append 'b'; ()
      case '\f'    => sb append '\\'; sb append 'f'; ()
      case '\n'    => sb append '\\'; sb append 'n'; ()
      case '\r'    => sb append '\\'; sb append 'r'; ()
      case '\t'    => sb append '\\'; sb append 't'; ()
      case '\\'    => sb append '\\'; sb append '\\'; ()
      case `other` => sb append '\\'; sb append c; ()
      case _ =>
        // if (c >= ' ' && c <= '~')
        if (!c.isControl)
          sb append c
        else {
          val hex = Integer.toHexString(c.toInt)
          sb append "\\u"
          hex.length match {
            case 1 => sb append "000"
            case 2 => sb append "00"
            case 3 => sb append '0'
            case _ =>
          }
          sb append hex
        }
        ()
    }

  trait Instances {
//    implicit def testStateDisplayUnit   : Display[Unit   ] = byToString
//    implicit val testStateDisplayBoolean: Display[Boolean] = byToString
//    implicit val testStateDisplayInt    : Display[Int    ] = byToString
//    implicit def testStateDisplayLong   : Display[Long   ] = byToString
//    implicit def testStateDisplayShort  : Display[Short  ] = byToString
//    implicit def testStateDisplayByte   : Display[Byte   ] = byToString

    implicit val testStateDisplayString: Display[String] =
      build { (sb, s) =>
        if (s.contains('\n')) {
          sb.append(s)
        } else {
          sb append '"'
          s.foreach(escapeChar(sb, '"'))
          sb append '"'
        }
        ()
      }

    implicit val testStateDisplayChar: Display[Char] =
      build { (sb, c) =>
        sb append '\''
        escapeChar(sb, '\'')(c)
        sb append '\''
        ()
      }

    implicit def testStateDisplayName: Display[Name] =
      Display(_.value)

    implicit def testStateDisplayNameFn[A]: Display[NameFn[A]] =
      Display.by(_(None))

    implicit def testStateDisplayOption[A](implicit display: Display[A]): Display[Option[A]] =
      Display {
        case None    => "None"
        case Some(a) => s"Some(${display(a)})"
      }

    implicit def testStateDisplayTraversable[C[X] <: Iterable[X], A](implicit display: Display[A]): Display[C[A]] =
      Display(_.iterator.map(display(_)).mkString(", "))
  }

  trait ToOps {
    implicit def toTestStateDisplayOps[A: Display](a: A): Ops[A] =
      new Ops(a)
  }

  object ToOps extends ToOps

  trait Implicits extends Instances with ToOps
}
