package teststate.cp3

import teststate.OS

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

  object Instances {

    import Types._
    import Profunctor.ToOps._
    import ToOps._

    type Check[C[_, _]] = ({ type T[O, S, E] = C[OS[O, S], E] })

    implicit def checkOpsInstanceByProfunctor[C[_, _]: Profunctor]: CheckOps[Check[C]#T] = {
      type M[O, S, E] = Check[C]#T[O, S, E]
      new CheckOps[M] {
        override def mapOS[O, S, E, X, Y](c: M[O, S, E])(f: OS[X, Y] => OS[O, S]): M[X, Y, E] =
          c lmap f
      }
    }

    def checkOpsInstanceForChecks[C[_, _]: Profunctor]: CheckOps[CheckShape[C, ?, ?, ?]] = {
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
    implicit def toCheckOpsOps[C[_, _, _], O, S, E](c: C[O, S, E])(implicit tc: CheckOps[C]): Ops[C, O, S, E] =
      new Ops[C, O, S, E](c)(tc)

//    import Types._
//    type CheckShape [C[_, _], O, S, E] = CheckShapeA[C, OS[O, S], E]
//    implicit def toCheckOpsForCheckShapes[C[_, _], O, S, E](c: Points[O, S, E]) = toCheckOpsOps[Point, O, S, E](c)
//    implicit def toCheckOpsOpsP[O, S, E](c: Points[O, S, E]) = toCheckOpsOps[Point, O, S, E](c)
  }

  object ToOps extends ToOps
}
