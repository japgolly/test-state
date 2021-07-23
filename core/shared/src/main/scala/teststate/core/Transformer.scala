package teststate.core

import teststate.core.CoreExports._
import teststate.core.CoreExports2._
import teststate.data.Or
import teststate.typeclass.{ExecutionModel, ~~>}

case class Transformer[F [_], R , O , S , E ,
                       F2[_], R2, O2, S2, E2]
    (actions   : Actions[F, R, O, S, E] => Actions[F2, R2, O2, S2, E2],
     invariants: Invariants   [O, S, E] => Invariants     [O2, S2, E2],
     points    : Points       [O, S, E] => Points         [O2, S2, E2],
     arounds   : Arounds      [O, S, E] => Arounds        [O2, S2, E2])
    (implicit val f1: ExecutionModel[F], val f2: ExecutionModel[F2]) {

  def mapF[X[_]](f: F2 ~~> X)(implicit x: ExecutionModel[X]) =
    Transformer[F, R, O, S, E, X, R2, O2, S2, E2](
      actions(_) trans f, invariants, points, arounds)

  def mapR[X](f: X => R2) =
    Transformer[F, R, O, S, E, F2, X, O2, S2, E2](
      actions(_) mapR f, invariants, points, arounds)

  def mapOS[X, Y](f: X => O2, g: Y => S2)(h: (Y, S2) => Y) =
    Transformer[F, R, O, S, E, F2, R2, X, Y, E2](
      actions   (_).mapOS(f, g)(h),
      invariants(_).mapOS(f, g),
      points    (_).mapOS(f, g),
      arounds   (_).mapOS(f, g))

  def mapO[X](f: X => O2) =
    Transformer[F, R, O, S, E, F2, R2, X, S2, E2](
      actions   (_) mapO f,
      invariants(_) mapO f,
      points    (_) mapO f,
      arounds   (_) mapO f)

  def mapS[X](g: X => S2)(h: (X, S2) => X) =
    Transformer[F, R, O, S, E, F2, R2, O2, X, E2](
      actions   (_).mapS(g)(h),
      invariants(_).mapS(g),
      points    (_).mapS(g),
      arounds   (_).mapS(g))

  def mapE[X](f: E2 => X) =
    Transformer[F, R, O, S, E, F2, R2, O2, S2, X](
      actions   (_) mapE f,
      invariants(_) mapE f,
      points    (_) mapE f,
      arounds   (_) mapE f)

  def pmapR[X](f: X => E2 Or R2) =
    Transformer[F, R, O, S, E, F2, X, O2, S2, E2](
      actions(_) pmapR f, invariants, points, arounds)

  def pmapO[X](f: X => E2 Or O2) =
    Transformer[F, R, O, S, E, F2, R2, X, S2, E2](
      actions   (_) pmapO f,
      invariants(_) pmapO f,
      points    (_) pmapO f,
      arounds   (_) pmapO f)

  // -------------------------------------------------------------------------------------------------------------------

  def cmapF[X[_]](f: X ~~> F)(implicit x: ExecutionModel[X]) =
    Transformer[X, R, O, S, E, F2, R2, O2, S2, E2](
      _.trans(f) |> actions, invariants, points, arounds)

  def cmapR[X](f: R => X) =
    Transformer[F, X, O, S, E, F2, R2, O2, S2, E2](
      _.mapR(f) |> actions, invariants, points, arounds)

  def cmapOS[X, Y](f: O => X, g: S => Y)(h: (S, Y) => S) =
    Transformer[F, R, X, Y, E, F2, R2, O2, S2, E2](
      _.mapOS(f, g)(h) |> actions   ,
      _.mapOS(f, g)    |> invariants,
      _.mapOS(f, g)    |> points    ,
      _.mapOS(f, g)    |> arounds   )

  def cmapO[X](f: O => X) =
    Transformer[F, R, X, S, E, F2, R2, O2, S2, E2](
      _.mapO(f) |> actions   ,
      _.mapO(f) |> invariants,
      _.mapO(f) |> points    ,
      _.mapO(f) |> arounds   )

  def cmapS[X](g: S => X)(h: (S, X) => S) =
    Transformer[F, R, O, X, E, F2, R2, O2, S2, E2](
      _.mapS(g)(h) |> actions   ,
      _.mapS(g)    |> invariants,
      _.mapS(g)    |> points    ,
      _.mapS(g)    |> arounds   )

  def cmapE[X](f: X => E) =
    Transformer[F, R, O, S, X, F2, R2, O2, S2, E2](
      _.mapE(f) |> actions   ,
      _.mapE(f) |> invariants,
      _.mapE(f) |> points    ,
      _.mapE(f) |> arounds   )

  def cpmapR[X](f: R => E Or X) =
    Transformer[F, X, O, S, E, F2, R2, O2, S2, E2](
      _.pmapR(f) |> actions, invariants, points, arounds)

  def cpmapO[X](f: O => E Or X) =
    Transformer[F, R, X, S, E, F2, R2, O2, S2, E2](
      _.pmapO(f) |> actions   ,
      _.pmapO(f) |> invariants,
      _.pmapO(f) |> points    ,
      _.pmapO(f) |> arounds   )

//  object Auto {
//    implicit def autoTransformActions   [F[_], R, O, S, E, F2[_], R2, O2, S2, E2](a: Actions[F, R, O, S, E])(implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Actions   [F2, R2, O2, S2, E2] = a.lift
//    implicit def autoTransformInvariants[F[_], R, O, S, E, F2[_], R2, O2, S2, E2](a: Invariants[O, S, E])   (implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Invariants[O2, S2, E2] = a.lift
//    implicit def autoTransformPoints    [F[_], R, O, S, E, F2[_], R2, O2, S2, E2](a: Points    [O, S, E])   (implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Points    [O2, S2, E2] = a.lift
//    implicit def autoTransformArounds   [F[_], R, O, S, E, F2[_], R2, O2, S2, E2](a: Arounds   [O, S, E])   (implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Arounds   [O2, S2, E2] = a.lift
//
//    implicit def autoTransformActions   (x: Actions[F, R, O, S, E]): Actions   [F2, R2, O2, S2, E2] = action   (x)
//    implicit def autoTransformInvariants(x: Invariants[O, S, E])   : Invariants[O2, S2, E2]         = invariant(x)
//    implicit def autoTransformPoints    (x: Points    [O, S, E])   : Points    [O2, S2, E2]         = point    (x)
//    implicit def autoTransformArounds   (x: Arounds   [O, S, E])   : Arounds   [O2, S2, E2]         = around   (x)
//  }
}

object Transformer {

  def id[F[_], R, O, S, E](implicit f: ExecutionModel[F]) =
    Transformer[F, R, O, S, E, F, R, O, S, E](
      identity, identity, identity, identity)(f, f)

  final class ActionOps[F[_], R, O, S, E](private val self: Actions[F, R, O, S, E]) extends AnyVal {
    def lift[F2[_], R2, O2, S2, E2](implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Actions[F2, R2, O2, S2, E2] =
      t.actions(self)
  }

  final class InvariantsOps[O, S, E](private val self: Invariants[O, S, E]) extends AnyVal {
    def lift[F[_], R, F2[_], R2, O2, S2, E2](implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Invariants[O2, S2, E2] =
      t.invariants(self)
  }

  final class PointsOps[O, S, E](private val self: Points[O, S, E]) extends AnyVal {
    def lift[F[_], R, F2[_], R2, O2, S2, E2](implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Points[O2, S2, E2] =
      t.points(self)
  }

  final class AroundsOps[O, S, E](private val self: Arounds[O, S, E]) extends AnyVal {
    def lift[F[_], R, F2[_], R2, O2, S2, E2](implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Arounds[O2, S2, E2] =
      t.arounds(self)
  }

  trait ToOps {
    implicit def toActionTransformerOps[F[_], R, O, S, E](a: Actions[F, R, O, S, E]): ActionOps[F, R, O, S, E] = new ActionOps(a)
    implicit def toInvariantsTransformerOps[O, S, E](a: Invariants[O, S, E]): InvariantsOps[O, S, E] = new InvariantsOps(a)
    implicit def toPointsTransformerOps    [O, S, E](a: Points    [O, S, E]): PointsOps    [O, S, E] = new PointsOps    (a)
    implicit def toAroundsTransformerOps   [O, S, E](a: Arounds   [O, S, E]): AroundsOps   [O, S, E] = new AroundsOps   (a)
  }
}
