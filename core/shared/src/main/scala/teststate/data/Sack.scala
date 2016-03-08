package teststate.data

import acyclic.file
import teststate.typeclass._
import Profunctor.ToOps.toProfunctorOps
import Sack._

sealed abstract class Sack[-I, +A] {

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

  final case class Value[+A](value: A) extends Sack[Any, A]

  final case class Product[-I, +A](contents: Vector[Sack[I, A]]) extends Sack[I, A]

  final case class CoProduct[-I, +A](name: NameFn[I], produce: I => Sack[I, A]) extends Sack[I, A]

  val empty = Product(Vector.empty)

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
}

