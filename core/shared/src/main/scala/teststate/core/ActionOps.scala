package teststate.core

import acyclic.file
import teststate.typeclass._
import Around.{When, Before, BeforeAndAfter, After}
import Profunctor.ToOps._
import Types._
import Action._
import Profunctor.ToOps._

/*

+ Conditional

Action Ops
==========
def trans[G[_]](t: F ~~> G)
def nameMod(f: Name => Name)
def rename(newName: NameFn[ROS[R, O, S]])
def addCheck(c: Arounds[O, S, E])
def modS(f: S => S)(implicit em: ExecutionModel[F])
def times(n: Int)
def mapR[RR](f: RR => R)
def pmapR[RR](f: RR => E Or R)(implicit em: ExecutionModel[F])
def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F])
def mapE[EE](f: E => EE)(implicit em: ExecutionModel[F])
def pmapO[OO](f: OO => E Or O)(implicit em: ExecutionModel[F])
def mapO[X](g: X => O)(implicit em: ExecutionModel[F])
def unzoomS[SS](s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F])


Actions Ops
===========

def >>(next: Action[F, R, O, S, E])
def andThen(next: Action[F, R, O, S, E])

*/

//def >>(next: Actions[F, R, O, S, E]): Actions[F, R, O, S, E] =
//object ActionOps {
//
//  final class Ops[F, R, O, S, E](private val self: Actions[F, R, O, S, E]) extends AnyVal {
//
//    def trans[G[_]](t: F ~~> G): Actions[G, R, O, S, E] =
//      self rmap {
//        case Single (n, r, c) => Single(n, run(_).map(f => () => t(f())), c)
//        case Group  (n, a, c) => Group(n, a(_).map(_ trans t), c)
//        case SubTest(n, a, i) => SubTest(n, a when f, i)
//      }
//  }
//
//  trait Implicits {
//    implicit def actionsToActionsOps[F, R, O, S, E](a: Actions[F, R, O, S, E]): Ops[F, R, O, S, E] =
//      new Ops(a)
//  }
//}

trait ActionOps[A[_[_], _, _, _, _]] {

  def trans[F[_], R, O, S, E, G[_]](a: A[F, R, O, S, E])(t: F ~~> G): A[G, R, O, S, E]
}

object ActionOps {

  final class Ops[A[_[_], _, _, _, _], F[_], R, O, S, E](private val a: A[F, R, O, S, E])(implicit tc: ActionOps[A]) {

    def trans[G[_]](t: F ~~> G): A[G, R, O, S, E] =
      tc.trans(a)(t)
  }

  trait Instances {

//    implicit val singleActionInstanceActionOps: ActionOps[Single] =
//      new ActionOps[Single] {
//
//        override def trans[F[_], R, O, S, E, G[_]](a: Single[F, R, O, S, E])(t: F ~~> G) =
//          a.copy(run = a.run(_).map(f => () => t(f())))
//      }

    implicit val actionInstanceActionOps: ActionOps[Action] =
      new ActionOps[Action] {

        override def trans[F[_], R, O, S, E, G[_]](x: Action[F, R, O, S, E])(t: F ~~> G) =
          x match {
            case Single (n, r, c) => Single (n, r(_).map(f => () => t(f())), c)
            case Group  (n, a, c) => Group  (n, a(_).map(_ trans t), c)
            case SubTest(n, a, i) => SubTest(n, a trans t, i)
          }
      }

    implicit lazy val actionsInstanceActionOps: ActionOps[Actions] =
      new ActionOps[Actions] {
        override def trans[F[_], R, O, S, E, G[_]](a: Actions[F, R, O, S, E])(t: F ~~> G) =
          a.rmap(_.map(_ trans t))
      }

    implicit def toActionOps[A[_[_], _, _, _, _], F[_], R, O, S, E](a: A[F, R, O, S, E])(implicit tc: ActionOps[A]): Ops[A, F, R, O, S, E] =
      new Ops(a)(tc)

  }

  trait ToOps {
  }

}
