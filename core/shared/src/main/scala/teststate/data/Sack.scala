package teststate.data

import japgolly.microlibs.name_fn._
import teststate.typeclass.Profunctor.ToOps.toProfunctorOps
import teststate.typeclass._

sealed abstract class Sack[-I, +A] {

//  def isEmpty(i: I): Boolean

  /** Whether this is provably empty.
    *
    * A coproduct could return an empty set for some inputs but it isn't provable and so isn't considered empty.
    */
  def isEmpty: Boolean

  final def nonEmpty = !isEmpty
}

object Sack {

  final case class Value[+A](value: A) extends Sack[Any, A] {
    override def isEmpty = false
  }

  final case class Product[-I, +A](contents: Vector[Sack[I, A]]) extends Sack[I, A] {
    override def isEmpty = contents.isEmpty
  }

  final case class CoProduct[-I, +A](name: NameFn[I], produce: I => Sack[I, A]) extends Sack[I, A] {
    override def isEmpty = false
  }

  val empty: Sack[Any, Nothing] =
    Product(Vector.empty)

  implicit val sackInstanceEmpty: Empty[Sack[Any, Nothing]] =
    Empty(Sack.empty)

  def append[A, B](a: Sack[A, B], b: Sack[A, B]): Sack[A, B] =
    if (a.isEmpty)
      b
    else if (b.isEmpty)
      a
    else
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
    Conditional((s, f) => s.rmap(c.when(_, f)))

  implicit def sackInstanceDisplay[A, B](implicit display: Display[B]): Display[Sack[A, B]] =
    Display { sack =>
      val sb = new StringBuilder
      def add(s: String): Unit = {
        if (sb.nonEmpty)
          sb append '\n'
        sb append s
        ()
      }
      def go(s: Sack[A, B]): Unit =
        s match {
          case Value(b)        => add(display(b))
          case Product(ss)     => ss foreach go
          case CoProduct(n, _) => add(n(None).value)
        }
      go(sack)
      sb.result()
    }
}

