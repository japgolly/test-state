package teststate.cp3

import teststate.OS
import Profunctor.ToOps._
import Types._

trait CheckOps[C[_, _, _]] {
  def mapOS[O, S, E, X, Y](c: C[O, S, E])(f: OS[X, Y] => OS[O, S]): C[X, Y, E]
}

object CheckOps {

  final class Ops[C[_, _, _], O, S, E](c: C[O, S, E])(implicit tc: CheckOps[C]) {

    def mapOS[X, Y](f: OS[X, Y] => OS[O, S]): C[X, Y, E] =
      tc.mapOS(c)(f)

    def mapO[X](f: X => O): C[X, S, E] =
      mapOS(_ mapO f)

    def mapS[X](f: X => S): C[O, X, E] =
      mapOS(_ mapS f)
  }

  trait Instances {

    private def checkOpsInstanceByProfunctor[C[_, _]: Profunctor]: CheckOps[λ[(O, S, E) => C[OS[O, S], E]]] = {
      type M[O, S, E] = C[OS[O, S], E]
      new CheckOps[M] {
        override def mapOS[O, S, E, X, Y](c: M[O, S, E])(f: OS[X, Y] => OS[O, S]): M[X, Y, E] =
          c lmap f
      }
    }

    private def checkOpsInstanceForChecks[C[_, _]: Profunctor]: CheckOps[CheckShape[C, ?, ?, ?]] = {
      val sub = checkOpsInstanceByProfunctor[C]
      new CheckOps[CheckShape[C, ?, ?, ?]] {
        override def mapOS[O, S, E, X, Y](c: CheckShape[C, O, S, E])(f: OS[X, Y] => OS[O, S]): CheckShape[C, X, Y, E] =
          c.dimap(_ map f, sub.mapOS(_)(f))
      }
    }

    implicit val checkOpsInstanceForPoints     = checkOpsInstanceForChecks[Point    ]
    implicit val checkOpsInstanceForArounds    = checkOpsInstanceForChecks[Around   ]
    implicit val checkOpsInstanceForInvariants = checkOpsInstanceForChecks[Invariant]
  }

  trait ToOps {
    implicit def toCheckOps[C[_, _, _], O, S, E](c: C[O, S, E])(implicit tc: CheckOps[C]): Ops[C, O, S, E] =
      new Ops[C, O, S, E](c)(tc)
  }

  trait Implicits extends Instances with ToOps
}
