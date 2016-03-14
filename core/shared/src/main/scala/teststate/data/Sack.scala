package teststate.data

import acyclic.file
import teststate.typeclass._
import Profunctor.ToOps.toProfunctorOps
import Sack._

sealed abstract class Sack[-I, +A] {

//  should be def isEmpty(i: I): Boolean
//  def isEmpty: Boolean
//
//  final def nonEmpty = !isEmpty

  // Note: Result is flat, coproudcts are resolved and flattened; their names discarded.
  final def foreach(i: I)(err: (Name, Throwable) => Unit)(f: A => Unit): Unit = {
    def go(s: Sack[I, A]): Unit =
      s match {
        case Value(a)        => f(a)
        case Product(ss)     => ss foreach go
        case CoProduct(n, p) => Recover.id.attempt(p(i)) match {
          case Right(s) => go(s)
          case Left(e)  => err(Recover.recoverToString.name(n, Some(i)), e)
        }
      }
    go(this)
  }
}

object Sack {

  final case class Value[+A](value: A) extends Sack[Any, A] {
//    override def isEmpty = false
  }

  final case class Product[-I, +A](contents: Vector[Sack[I, A]]) extends Sack[I, A] {
//    override def isEmpty = contents.isEmpty
  }

  final case class CoProduct[-I, +A](name: NameFn[I], produce: I => Sack[I, A]) extends Sack[I, A] {
//    override def isEmpty = false
  }

  val empty = Product(Vector.empty)

  def append[A, B](a: Sack[A, B], b: Sack[A, B]): Sack[A, B] =
//    if (a.isEmpty)
//      b
//    else if (b.isEmpty)
//      a
//    else
    (a, b) match {
      case (Product(p), Product(q)) => Product(p ++ q)
      case (p         , Product(q)) => Product(p +: q)
      case (Product(p), q         ) => Product(p :+ q)
      case (p         , q         ) => Product(Vector.empty :+ p :+ q)
    }

  implicit val sackInstanceProfunctor: Profunctor[Sack] =
    new Profunctor[Sack] {

      override def rmap[A, B, C](m: Sack[A, B])(f: B => C): Sack[A, C] =
        m match {
          case Value(b)        => Value(f(b))
          case Product(s)      => Product(s map (_ rmap f))
          case CoProduct(n, p) => CoProduct(n, p(_) rmap f)
        }

      override def dimap[A, B, C, D](m: Sack[A, B])(g: C => A, f: B => D): Sack[C, D] =
        m match {
          case Value(b)        => Value(f(b))
          case Product(s)      => Product(s map (_.dimap(g, f)))
          case CoProduct(n, p) => CoProduct(n cmap g, c => p(g(c)).dimap(g, f))
        }
    }

  implicit def sackInstanceConditionalR[A, B, I](implicit c: Conditional[B, I]): Conditional[Sack[A, B], I] =
    new Conditional[Sack[A, B], I] {
      override def when(m: Sack[A, B], f: I => Boolean) =
        m.rmap(c.when(_, f))
    }

  implicit def sackInstanceShow[A, B](implicit show: Show[B]): Show[Sack[A, B]] =
    Show { sack =>
      val sb = new StringBuilder
      def add(s: String): Unit = {
        if (sb.nonEmpty)
          sb append '\n'
        sb append s
        ()
      }
      def go(s: Sack[A, B]): Unit =
        s match {
          case Value(b)        => add(show(b))
          case Product(ss)     => ss foreach go
          case CoProduct(n, _) => add(n(None).value)
        }
      go(sack)
      sb.result()
    }

}

