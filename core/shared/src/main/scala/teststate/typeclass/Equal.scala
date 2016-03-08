package teststate.typeclass

import acyclic.file

class Equal[A](val equal: (A, A) => Boolean) extends AnyVal

object Equal {

  def apply[A](fn: (A, A) => Boolean): Equal[A] =
    new Equal(fn)

  def by_==[A]: Equal[A] =
    Equal(_ == _)

  implicit val equalUnit   : Equal[Unit   ] = by_==
  implicit val equalChar   : Equal[Char   ] = by_==
  implicit val equalString : Equal[String ] = by_==
  implicit val equalBoolean: Equal[Boolean] = by_==
  implicit val equalInt    : Equal[Int    ] = by_==
  implicit val equalLong   : Equal[Long   ] = by_==
  implicit val equalShort  : Equal[Short  ] = by_==
  implicit val equalByte   : Equal[Byte   ] = by_==

  implicit def equalOption[A](implicit e: Equal[A]): Equal[Option[A]] =
    Equal((a, b) => a match {
      case None    => b.isEmpty
      case Some(x) => b.exists(e.equal(x, _))
    })
}
