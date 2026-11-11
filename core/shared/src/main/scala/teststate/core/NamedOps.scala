package teststate.core

import japgolly.microlibs.name_fn.Name.Implicits._
import japgolly.microlibs.name_fn._
import teststate.data._

case class NamedOps[A, I](renameBy: (A, NameFn[I] => NameFn[I]) => A) extends AnyVal

object NamedOps {
  final class Ops[A, I](self: A)(implicit tc: NamedOps[A, I]) {

    def renameBy(f: NameFn[I] => NameFn[I]): A =
      tc.renameBy(self, f)

    def rename(newName: NameFn[I]): A =
      renameBy(_ => newName)

    def renameContextFree(n: Name): A =
      renameBy(_ mapContextFree n)

    def nameMod(f: Name => Name): A =
      renameBy(_ map f)

    def prefix(p: => String): A =
      nameMod(p + _.value)

    def suffix(s: => String): A =
      nameMod(_.value + s)
  }

  trait ToOps {
    implicit def toTestStateNamedOps[A, I](a: A)(implicit n: NamedOps[A, I]): Ops[A, I] =
      new Ops(a)(n)
  }

  object ToOps extends ToOps

  trait Instances {

//    implicit def sackInstanceNamedOps[A, B, I](implicit n: NamedOps[B, I]): NamedOps[Sack[A, B], I] =
//      NamedOps((s, f) => s.rmap(n.renameBy(_, f)))

    implicit def sackInstanceNamedOps[A, B](implicit nb: NamedOps[B, A]): NamedOps[Sack[A, B], A] =
      NamedOps { (s, f) =>
        def go(s: Sack[A, B]): Sack[A, B] =
          s match {
            case Sack.Value(b)        => Sack.Value(nb.renameBy(b, f))
            case Sack.Product(ss)     => Sack.Product(ss map go)
            case Sack.CoProduct(n, p) => Sack.CoProduct(f(n), p andThen go)
          }
        go(s)
      }

    implicit def sackInstanceNamedOpsBA[A, B, I](implicit nb: NamedOps[B, BeforeAfter[A]]): NamedOps[Sack[A, B], BeforeAfter[A]] =
      NamedOps { (s, f) =>
        def go(s: Sack[A, B]): Sack[A, B] =
          s match {
            case Sack.Value(b)        => Sack.Value(nb.renameBy(b, f))
            case Sack.Product(ss)     => Sack.Product(ss map go)
            case Sack.CoProduct(n, p) => Sack.CoProduct(f.thruBefore(n), p andThen go)
          }
        go(s)
      }

    implicit def namedErrorOrXInstanceNamedOps[A, I, E](implicit n: NamedOps[A, I]): NamedOps[NamedError[E] Or A, I] =
      NamedOps((o, f) =>
        o match {
          case Right(a) => Right(n.renameBy(a, f))
          case Left(e)  => Left(NamedError(f(NameFn.const(e.name))(None), e.error))
        }
      )
  }

  trait Implicits extends Instances with ToOps
}
