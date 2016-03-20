package teststate.typeclass

import acyclic.file
import teststate.data.{Or, Skipped, Tri}

case class Conditional[M, I](when: (M, I => Boolean) => M) extends AnyVal

object Conditional {

  final class Ops[M, I](m: M)(implicit c: Conditional[M, I]) {

    def when(f: I => Boolean): M =
      c.when(m, f)

    def unless(f: I => Boolean): M =
      when(!f(_))

    def skip: M =
      when(_ => false)
  }

  trait Instances {
    implicit def conditionalFnToTri[I, E, A]: Conditional[I => Tri[E, A], I] =
      Conditional((m, f) => i => if (f(i)) m(i) else Skipped)

    implicit def conditionalFnToOption[I, A]: Conditional[I => Option[A], I] =
      Conditional((m, f) => i => if (f(i)) m(i) else None)

    implicit def conditionalRight[L, R, I](implicit c: Conditional[R, I]): Conditional[L Or R, I] =
      Conditional((m, f) => m.map(c.when(_, f)))
  }

  trait ToOps {
    implicit def toConditionalOps[M, I](m: M)(implicit c: Conditional[M, I]): Ops[M, I] =
      new Ops(m)(c)
  }

  trait Implicits extends Instances with ToOps

  object Implicits extends Implicits
}

