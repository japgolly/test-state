package teststate.typeclass

import acyclic.file

final class Show[A](private val show: A => String) extends AnyVal {
  def apply(a: A): String =
    show(a)

  def cmap[B](f: B => A): Show[B] =
    Show(b => show(f(b)))

  def map(f: String => String): Show[A] =
    Show(a => f(show(a)))

  def indent: Show[A] =
    indent("  ")

  def indent(i: String): Show[A] =
    map(i + _.replace("\n", "\n" + i))

  def mkString[C[X] <: TraversableOnce[X]](start: String, mid: String, end: String): Show[C[A]] =
    Show(_.toIterator.map(show).mkString(start, mid, end))

  def coll[C[X] <: TraversableOnce[X]]: Show[C[A]] =
    mkString("[", ", ", "]")
}

object Show {
  final class Ops[A](private val a: A)(implicit s: Show[A]) {
    def show: String =
      s(a)
  }

  def apply[A](f: A => String): Show[A] =
    new Show(f)

  def byToString[A]: Show[A] =
    Show(_.toString)

  def build[A](f: (StringBuilder, A) => Unit): Show[A] =
    Show { a =>
      val sb = new StringBuilder
      f(sb, a)
      sb.result()
    }

  // Putting this into a low-priority implicits trait overrides implicit instances in user-defined companion objects. :(
  implicit def showByToString[A]: Show[A] =
    Show(_.toString)

  def escapeChar(sb: StringBuilder, other: Char)(c: Char): Unit =
    if (c < ' ')
      c match {
        case '\b' => sb append '\\'; sb append 'b'; ()
        case '\f' => sb append '\\'; sb append 'f'; ()
        case '\n' => sb append '\\'; sb append 'n'; ()
        case '\r' => sb append '\\'; sb append 'r'; ()
        case '\t' => sb append '\\'; sb append 't'; ()
        case _    =>
          val i = c.toInt
          sb append "\\u00"
          if (i < 16) sb append '0'
          sb append Integer.toHexString(i)
          ()
      }
    else
      c match {
        case '\\'    => sb append '\\'; sb append '\\'; ()
        case `other` => sb append '\\'; sb append c   ; ()
        case _       => sb append c                   ; ()
      }

  trait Instances {
//    implicit def showUnit   : Show[Unit   ] = byToString
//    implicit val showBoolean: Show[Boolean] = byToString
//    implicit val showInt    : Show[Int    ] = byToString
//    implicit def showLong   : Show[Long   ] = byToString
//    implicit def showShort  : Show[Short  ] = byToString
//    implicit def showByte   : Show[Byte   ] = byToString

    implicit val showString: Show[String] =
      build { (sb, s) =>
        sb append '"'
        s.foreach(escapeChar(sb, '"'))
        sb append '"'
        ()
      }

    implicit val showChar: Show[Char] =
      build { (sb, c) =>
        sb append '\''
        escapeChar(sb, '\'')(c)
        sb append '\''
        ()
      }

    implicit def showOption[A](implicit show: Show[A]): Show[Option[A]] =
      Show {
        case None    => "None"
        case Some(a) => s"Some(${show(a)})"
      }

    implicit def showTraversable[C[X] <: Traversable[X], A](implicit show: Show[A]): Show[C[A]] =
      Show(_.toIterator.map(show(_)).mkString(", "))
  }

  trait ToOps {
    implicit def toShowOps[A: Show](a: A): Ops[A] =
      new Ops(a)
  }

  object ToOps extends ToOps

  trait Implicits extends Instances with ToOps
}
