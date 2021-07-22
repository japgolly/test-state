package teststate.core

import teststate.data._
import Name.Implicits._

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
  import ToOps._

  trait Instances {

//    implicit def sackInstanceNamedOps[A, B, I](implicit n: NamedOps[B, I]): NamedOps[Sack[A, B], I] =
//      NamedOps((s, f) => s.rmap(n.renameBy(_, f)))

    implicit def sackInstanceNamedOps[A, B](implicit nb: NamedOps[B, A]): NamedOps[Sack[A, B], A] =
      NamedOps((s, f) =>
        s match {
          case Sack.Value(b)        => Sack.Value(nb.renameBy(b, f))
          case Sack.Product(ss)     => Sack.Product(ss map (_ renameBy f))
          case Sack.CoProduct(n, p) => Sack.CoProduct(f(n), p)
        }
      )

    implicit def sackInstanceNamedOpsBA[A, B, I](implicit nb: NamedOps[B, BeforeAfter[A]]): NamedOps[Sack[A, B], BeforeAfter[A]] =
      NamedOps((s, f) =>
        s match {
          case Sack.Value(b)        => Sack.Value(nb.renameBy(b, f))
          case Sack.Product(ss)     => Sack.Product(ss map (_ renameBy f))
          case Sack.CoProduct(n, p) => Sack.CoProduct(f.thruBefore(n), p)
        }
      )

    implicit def namedErrorOrXInstanceNamedOps[A, I, E](implicit n: NamedOps[A, I]): NamedOps[NamedError[E] Or A, I] =
      NamedOps((o, f) => o.map(n.renameBy(_, f)))
  }

  trait Implicits extends Instances with ToOps
}