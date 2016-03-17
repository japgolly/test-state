package teststate.core

import acyclic.file
import teststate.data.Sack
import teststate.typeclass.PolyComposable
import Action.Actions
import CoreExports._
import CoreExports2._
import PolyComposable.CanSeq

/**
  * P = Point
  * @ = Around
  * I = Invariant
  * C = Check
  * A = Action
  *
  * C & A = -
  * A & C = -
  *
  * A >> A = A
  * A >> P = A with post-condition
  * A >> @ = A with post-condition
  * A >> I = -
  *
  * P >> A = A with pre-condition
  * P >> P = -
  * P >> @ = -
  * P >> I = -
  *
  * @ >> A = -
  * @ >> P = -
  * @ >> @ = -
  * @ >> I = -
  *
  * I >> A = -
  * I >> P = -
  * I >> @ = -
  * I >> I = -
  */
object CoreComposition2 {

  trait Implicits {

    implicit def actionsMonoComposable[F[_], R, O, S, E]: PolyComposable.Mono[Actions[F, R, O, S, E]] =
      PolyComposable(Sack.append)

//    implicit def actionsComposePreCond[F[_], R, O, S, E]: PolyComposable.Right[Points[O, S, E], Actions[F, R, O, S, E]] =
//      PolyComposable((p, a) => a addCheck p.before)
//
//    implicit def actionsComposePostCond[F[_], R, O, S, E]: PolyComposable.Left[Actions[F, R, O, S, E], Points[O, S, E]] =
//      PolyComposable((a, p) => a addCheck p.after)
//
//    implicit def actionsComposePostCondA[F[_], R, O, S, E]: PolyComposable.Left[Actions[F, R, O, S, E], Arounds[O, S, E]] =
//      PolyComposable((a, c) => a addCheck c)

    import Sack._

    // Here be hacks... I want
    // (A1 >> @1 >> A2 >> @2) to be interpretted as
    // ((A1 >> @1) >> (A2 >> @2)) instead of
    // ((A1 >> @1 >> A2) >> @2).

    implicit def actionsComposePreCond[F[_], R, O, S, E]: PolyComposable.Right[Points[O, S, E], Actions[F, R, O, S, E]] =
      PolyComposable((p, a) => a match {
        case Product(ss) if ss.nonEmpty => Product((p >> ss.head) +: ss.tail)
        case _                          => a addCheck p.before
      })

    implicit def actionsComposePostCond[F[_], R, O, S, E]: PolyComposable.Left[Actions[F, R, O, S, E], Points[O, S, E]] =
      PolyComposable((a, p) => a match {
        case Product(ss) if ss.nonEmpty => Product(ss.init :+ (ss.last >> p))
        case _                          => a addCheck p.after
      })

    implicit def actionsComposePostCondA[F[_], R, O, S, E]: PolyComposable.Left[Actions[F, R, O, S, E], Arounds[O, S, E]] =
      PolyComposable((a, c) => a match {
        case Product(ss) if ss.nonEmpty => Product(ss.init :+ (ss.last >> c))
        case _                          => a addCheck c
      })

    implicit def actionsCanSeq[F[_], R, O, S, E]: CanSeq[Actions[F, R, O, S, E]] = CanSeq

    implicit def pointsCanSeq[O, S, E]: CanSeq[Points[O, S, E]] = CanSeq
  }

}
