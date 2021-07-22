package teststate.core

import teststate.data.Sack
import teststate.typeclass.PolyComposable
import Action.Actions
import CoreExports._
import CoreExports2._
import PolyComposable._

object CoreComposition2 {
  trait Implicits {

    implicit def actionsMonoComposable[F[_], R, O, S, E]: Mono[SeqOp, Actions[F, R, O, S, E]] =
      PolyComposable(Sack.append)

    implicit def pointsMonoComposable[O, S, E]: Mono[HPSeqOp, Points[O, S, E]] =
      PolyComposable(Sack.append)

    implicit def actionsComposePreCond[F[_], R, O, S, E]: Right[HPSeqOp, Points[O, S, E], Actions[F, R, O, S, E]] =
      PolyComposable((p, a) => a addCheck p.before)

    implicit def actionsComposePostCond[F[_], R, O, S, E]: Left[HPSeqOp, Actions[F, R, O, S, E], Points[O, S, E]] =
      PolyComposable((a, p) => a addCheck p.after)

    implicit def actionsComposePostCondA[F[_], R, O, S, E]: Left[HPSeqOp, Actions[F, R, O, S, E], Arounds[O, S, E]] =
      PolyComposable((a, c) => a addCheck c)

    implicit def actionsCanSeq  [F[_], R, O, S, E]: Can[SeqOp  , Actions[F, R, O, S, E]] = Can
    implicit def actionsCanSeqHP[F[_], R, O, S, E]: Can[HPSeqOp, Actions[F, R, O, S, E]] = Can

    implicit def pointsCanSeqHP[O, S, E]: Can[HPSeqOp, Points[O, S, E]] = Can
  }

}
