package teststate.typeclass

import acyclic.file

case class Show[A](private val show: A => String) /*extends AnyVal*/ {
  def apply(a: A): String =
    show(a)

  def map(f: String => String): Show[A] =
    Show(a => f(show(a)))

  def mkString[C[X] <: TraversableOnce[X]](start: String, mid: String, end: String): Show[C[A]] =
    Show(_.toIterator.map(show).mkString(start, mid, end))

  def coll[C[X] <: TraversableOnce[X]]: Show[C[A]] =
    mkString("[", ", ", "]")
}

object Show {
  def byToString[A]: Show[A] =
    Show(_.toString)

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

  object Implicits {
    implicit def showByToString[A]: Show[A] =
      Show(_.toString)
  }
}
