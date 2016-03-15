package teststate.core

import acyclic.file
import CoreExports._
import CoreExports2._
import teststate.core.Types.CheckShape
import teststate.data.{Or, OS}
import teststate.typeclass.{~~>, ExecutionModel}

import Transformer._

case class Transformer
  [F [_], R , O , S , E ,
   F2[_], R2, O2, S2, E2]
  (action   : Actions[F, R, O, S, E] => Actions[F2, R2, O2, S2, E2],
   invariant: Invariants   [O, S, E] => Invariants     [O2, S2, E2],
   point    : Points       [O, S, E] => Points         [O2, S2, E2],
   around   : Arounds      [O, S, E] => Arounds        [O2, S2, E2])
  (implicit f1: ExecutionModel[F], f2: ExecutionModel[F2]) {

  def mapF[X[_]](f: F2 ~~> X)(implicit x: ExecutionModel[X]) =
    Transformer[F, R, O, S, E, X, R2, O2, S2, E2](
      action(_) trans f, invariant, point, around)

  def mapR[X](f: X => R2) =
    Transformer[F, R, O, S, E, F2, X, O2, S2, E2](
      action(_) mapR f, invariant, point, around)

  def mapOS[X, Y](f: X => O2, g: Y => S2)(h: (Y, S2) => Y) =
    Transformer[F, R, O, S, E, F2, R2, X, Y, E2](
      action   (_).mapOS(f, g)(h),
      invariant(_).mapOS(f, g),
      point    (_).mapOS(f, g),
      around   (_).mapOS(f, g))

  def mapO[X](f: X => O2) =
    Transformer[F, R, O, S, E, F2, R2, X, S2, E2](
      action   (_) mapO f,
      invariant(_) mapO f,
      point    (_) mapO f,
      around   (_) mapO f)

  def mapS[X](g: X => S2)(h: (X, S2) => X) =
    Transformer[F, R, O, S, E, F2, R2, O2, X, E2](
      action   (_).mapS(g)(h),
      invariant(_).mapS(g),
      point    (_).mapS(g),
      around   (_).mapS(g))

  def mapE[X](f: E2 => X) =
    Transformer[F, R, O, S, E, F2, R2, O2, S2, X](
      action   (_) mapE f,
      invariant(_) mapE f,
      point    (_) mapE f,
      around   (_) mapE f)

  def pmapR[X](f: X => E2 Or R2) =
    Transformer[F, R, O, S, E, F2, X, O2, S2, E2](
      action(_) pmapR f, invariant, point, around)

  def pmapO[X](f: X => E2 Or O2) =
    Transformer[F, R, O, S, E, F2, R2, X, S2, E2](
      action   (_) pmapO f,
      invariant(_) pmapO f,
      point    (_) pmapO f,
      around   (_) pmapO f)

  // -------------------------------------------------------------------------------------------------------------------

  def cmapF[X[_]](f: X ~~> F)(implicit x: ExecutionModel[X]) =
    Transformer[X, R, O, S, E, F2, R2, O2, S2, E2](
      _.trans(f) |> action, invariant, point, around)

  def cmapR[X](f: R => X) =
    Transformer[F, X, O, S, E, F2, R2, O2, S2, E2](
      _.mapR(f) |> action, invariant, point, around)

  def cmapOS[X, Y](f: O => X, g: S => Y)(h: (S, Y) => S) =
    Transformer[F, R, X, Y, E, F2, R2, O2, S2, E2](
      _.mapOS(f, g)(h) |> action   ,
      _.mapOS(f, g)    |> invariant,
      _.mapOS(f, g)    |> point    ,
      _.mapOS(f, g)    |> around   )

  def cmapO[X](f: O => X) =
    Transformer[F, R, X, S, E, F2, R2, O2, S2, E2](
      _.mapO(f) |> action   ,
      _.mapO(f) |> invariant,
      _.mapO(f) |> point    ,
      _.mapO(f) |> around   )

  def cmapS[X](g: S => X)(h: (S, X) => S) =
    Transformer[F, R, O, X, E, F2, R2, O2, S2, E2](
      _.mapS(g)(h) |> action   ,
      _.mapS(g)    |> invariant,
      _.mapS(g)    |> point    ,
      _.mapS(g)    |> around   )

  def cmapE[X](f: X => E) =
    Transformer[F, R, O, S, X, F2, R2, O2, S2, E2](
      _.mapE(f) |> action   ,
      _.mapE(f) |> invariant,
      _.mapE(f) |> point    ,
      _.mapE(f) |> around   )

  def cpmapR[X](f: R => E Or X) =
    Transformer[F, X, O, S, E, F2, R2, O2, S2, E2](
      _.pmapR(f) |> action, invariant, point, around)

  def cpmapO[X](f: O => E Or X) =
    Transformer[F, R, X, S, E, F2, R2, O2, S2, E2](
      _.pmapO(f) |> action   ,
      _.pmapO(f) |> invariant,
      _.pmapO(f) |> point    ,
      _.pmapO(f) |> around   )

  object Auto {
//    implicit def autoTransformActions   [F[_], R, O, S, E, F2[_], R2, O2, S2, E2](a: Actions[F, R, O, S, E])(implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Actions   [F2, R2, O2, S2, E2] = a.lift
//    implicit def autoTransformInvariants[F[_], R, O, S, E, F2[_], R2, O2, S2, E2](a: Invariants[O, S, E])   (implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Invariants[O2, S2, E2] = a.lift
//    implicit def autoTransformPoints    [F[_], R, O, S, E, F2[_], R2, O2, S2, E2](a: Points    [O, S, E])   (implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Points    [O2, S2, E2] = a.lift
//    implicit def autoTransformArounds   [F[_], R, O, S, E, F2[_], R2, O2, S2, E2](a: Arounds   [O, S, E])   (implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Arounds   [O2, S2, E2] = a.lift

    implicit def autoTransformActions   (x: Actions[F, R, O, S, E]): Actions   [F2, R2, O2, S2, E2] = action   (x)
    implicit def autoTransformInvariants(x: Invariants[O, S, E])   : Invariants[O2, S2, E2]         = invariant(x)
    implicit def autoTransformPoints    (x: Points    [O, S, E])   : Points    [O2, S2, E2]         = point    (x)
    implicit def autoTransformArounds   (x: Arounds   [O, S, E])   : Arounds   [O2, S2, E2]         = around   (x)
  }
}

object Transformer {

  def id[F[_], R, O, S, E](implicit f: ExecutionModel[F]) =
    Transformer[F, R, O, S, E, F, R, O, S, E](
      identity, identity, identity, identity)(f, f)

  final class ActionOps[F[_], R, O, S, E](private val self: Actions[F, R, O, S, E]) extends AnyVal {
    def lift[F2[_], R2, O2, S2, E2](implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Actions[F2, R2, O2, S2, E2] =
      t.action(self)
  }

  final class InvariantsOps[O, S, E](private val self: Invariants[O, S, E]) extends AnyVal {
    def lift[F[_], R, F2[_], R2, O2, S2, E2](implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Invariants[O2, S2, E2] =
      t.invariant(self)
  }

  final class PointsOps[O, S, E](private val self: Points[O, S, E]) extends AnyVal {
    def lift[F[_], R, F2[_], R2, O2, S2, E2](implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Points[O2, S2, E2] =
      t.point(self)
  }

  final class AroundsOps[O, S, E](private val self: Arounds[O, S, E]) extends AnyVal {
    def lift[F[_], R, F2[_], R2, O2, S2, E2](implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Arounds[O2, S2, E2] =
      t.around(self)
  }

  trait ToOps {
    implicit def toActionTransformerOps[F[_], R, O, S, E](a: Actions[F, R, O, S, E]): ActionOps[F, R, O, S, E] = new ActionOps(a)

    implicit def toInvariantsTransformerOps[O, S, E](a: Invariants[O, S, E]): InvariantsOps[O, S, E] = new InvariantsOps(a)
    implicit def toPointsTransformerOps    [O, S, E](a: Points    [O, S, E]): PointsOps    [O, S, E] = new PointsOps    (a)
    implicit def toAroundsTransformerOps   [O, S, E](a: Arounds   [O, S, E]): AroundsOps   [O, S, E] = new AroundsOps   (a)
  }
}
