package teststate.core

import teststate.core.Action.Actions
import teststate.core.CoreExports._
import teststate.core.CoreExports2._
import teststate.data.Sack
import teststate.typeclass.PolyComposable
import teststate.typeclass.PolyComposable._

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
