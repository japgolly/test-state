package teststate.typeclass

import acyclic.file

class Equal[A](val equal: (A, A) => Boolean) extends AnyVal

object Equal {

  def apply[A](fn: (A, A) => Boolean): Equal[A] =
    new Equal(fn)

  def by_==[A]: Equal[A] =
    Equal(_ == _)

  trait Implicits {
    implicit val equalUnit   : Equal[Unit   ] = by_==
    implicit val equalChar   : Equal[Char   ] = by_==
    implicit val equalString : Equal[String ] = by_==
    implicit val equalBoolean: Equal[Boolean] = by_==
    implicit val equalInt    : Equal[Int    ] = by_==
    implicit val equalLong   : Equal[Long   ] = by_==
    implicit def equalShort  : Equal[Short  ] = by_==
    implicit def equalByte   : Equal[Byte   ] = by_==

    implicit def equalOption[A](implicit e: Equal[A]): Equal[Option[A]] =
      Equal((a, b) => a match {
        case None    => b.isEmpty
        case Some(x) => b.exists(e.equal(x, _))
      })

    implicit def equalSeq[C[x] <: Seq[x], A](implicit e: Equal[A]): Equal[C[A]] =
      Equal((x, y) =>
        (x eq y)
          || x.corresponds(y)(e.equal))

    implicit def equalIndexedSeq[C[x] <: IndexedSeq[x], A](implicit e: Equal[A]): Equal[C[A]] =
      Equal((x, y) =>
        (x eq y)
          || (x.length == y.length && x.indices.forall(i => e.equal(x(i), y(i)))))
  }
}
