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

    def println(): Unit =
      System.out.println(show)
  }

  def apply[A](f: A => String): Show[A] =
    new Show(f)

  def byToString[A]: Show[A] =
    Show(_.toString)

  trait Instances {
    implicit val showUnit   : Show[Unit   ] = byToString
    implicit val showBoolean: Show[Boolean] = byToString
    implicit val showInt    : Show[Int    ] = byToString
    implicit val showLong   : Show[Long   ] = byToString
    implicit val showShort  : Show[Short  ] = byToString
    implicit val showByte   : Show[Byte   ] = byToString

    implicit val showString: Show[String] =
      Show[String](s =>
        // Handle \n, \t, spaces (so surrounds), long strings (?)
        "\"" + s + "\""
      )

    implicit val showChar: Show[Char] =
      Show[Char](s =>
        // Handle \n, \t, spaces (so surrounds), long strings (?)
        "'" + s + "'"
      )

    implicit def showOption[A](implicit show: Show[A]): Show[Option[A]] =
      Show {
        case None => "None"
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

  object OptionalImplicits {
    implicit def showByToString[A]: Show[A] =
      Show(_.toString)
  }
}
