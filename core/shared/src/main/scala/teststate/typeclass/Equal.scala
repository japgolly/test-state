package teststate.typeclass

import acyclic.file

class Equal[A](val equal: (A, A) => Boolean) extends AnyVal

object Equal {

  def apply[A](fn: (A, A) => Boolean): Equal[A] =
    new Equal(fn)

  def byUnivEq[A]: Equal[A] =
    Equal(_ == _)

  implicit val equalUnit   : Equal[Unit   ] = byUnivEq
  implicit val equalChar   : Equal[Char   ] = byUnivEq
  implicit val equalString : Equal[String ] = byUnivEq
  implicit val equalBoolean: Equal[Boolean] = byUnivEq
  implicit val equalInt    : Equal[Int    ] = byUnivEq
  implicit val equalLong   : Equal[Long   ] = byUnivEq
  implicit val equalShort  : Equal[Short  ] = byUnivEq
  implicit val equalByte   : Equal[Byte   ] = byUnivEq

  implicit def equalOption[A](implicit e: Equal[A]): Equal[Option[A]] =
    Equal((a, b) => a match {
      case None    => b.isEmpty
      case Some(x) => b.exists(e.equal(x, _))
    })
}
