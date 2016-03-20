package teststate.core

import acyclic.file
import teststate.data._
import teststate.typeclass._
import Action.{Actions => _, _}
import Profunctor.ToOps._
import CoreExports._

trait ActionOps[A[_[_], _, _, _, _]] {

  def trans[F[_], R, O, S, E, G[_]](a: A[F, R, O, S, E])(t: F ~~> G): A[G, R, O, S, E]

  def mapR[F[_], R, O, S, E, X](x: A[F, R, O, S, E])(f: X => R): A[F, X, O, S, E]

  def mapOS[F[_], R, O, S, E, X, Y](a: A[F, R, O, S, E])(f: X => O, g: Y => S)(h: (Y, S) => Y)(implicit em: ExecutionModel[F]): A[F, R, X, Y, E]

  def mapE[F[_], R, O, S, E, X](x: A[F, R, O, S, E])(f: E => X)(implicit em: ExecutionModel[F]): A[F, R, O, S, X]

  def pmapR[F[_], R, O, S, E, X](x: A[F, R, O, S, E])(f: X => E Or R)(implicit em: ExecutionModel[F]): A[F, X, O, S, E]

  def pmapO[F[_], R, O, S, E, X](x: A[F, R, O, S, E])(f: X => E Or O)(implicit em: ExecutionModel[F]): A[F, R, X, S, E]

  def modS[F[_], R, O, S, E](x: A[F, R, O, S, E])(f: O => S => E Or S)(implicit em: ExecutionModel[F]): A[F, R, O, S, E]
}

trait ActionOps2[A[_[_], _, _, _, _]] {

  def times[F[_], R, O, S, E](a: A[F, R, O, S, E])(n: Int): A[F, R, O, S, E]

  def addCheck[F[_], R, O, S, E](a: A[F, R, O, S, E])(c: Arounds[O, S, E]): A[F, R, O, S, E]
}

object ActionOps {

  final class Ops[A[_[_], _, _, _, _], F[_], R, O, S, E](private val a: A[F, R, O, S, E])(implicit tc: ActionOps[A]) {

    def trans[G[_]](t: F ~~> G) =
      tc.trans(a)(t)

    def mapR[X](f: X => R) =
      tc.mapR(a)(f)

    def mapOS[X, Y](f: X => O, g: Y => S)(h: (Y, S) => Y)(implicit em: ExecutionModel[F]) =
      tc.mapOS(a)(f, g)(h)

    def mapO[X](f: X => O)(implicit em: ExecutionModel[F]) =
      mapOS[X, S](f, identity)((_, s) => s)

    def mapS[X](g: X => S)(h: (X, S) => X)(implicit em: ExecutionModel[F]) =
      mapOS[O, X](identity, g)(h)

    def mapE[X](f: E => X)(implicit em: ExecutionModel[F]) =
      tc.mapE(a)(f)

    def pmapR[X](f: X => E Or R)(implicit em: ExecutionModel[F]) =
      tc.pmapR(a)(f)

    def pmapO[X](f: X => E Or O)(implicit em: ExecutionModel[F]) =
      tc.pmapO(a)(f)

    private[core] def modS(f: O => S => E Or S)(implicit em: ExecutionModel[F]) =
      tc.modS(a)(f)

    def updateState(f: S => S)(implicit em: ExecutionModel[F]) =
      tryUpdateState(s => Right(f(s)))

    def tryUpdateState(f: S => E Or S)(implicit em: ExecutionModel[F]) =
      modS(_ => f)

    def updateStateBy(f: OS[O, S] => S)(implicit em: ExecutionModel[F]) =
      tryUpdateStateBy(i => Right(f(i)))

    def tryUpdateStateBy(f: OS[O, S] => E Or S)(implicit em: ExecutionModel[F]) =
      modS(o => s => f(OS(o, s)))
  }

  final class Ops2[A[_[_], _, _, _, _], F[_], R, O, S, E](private val a: A[F, R, O, S, E])(implicit tc: ActionOps2[A]) {

    def times(n: Int) = {
      require(n >= 0, "n in action.times(n) cannot be negative.")
      tc.times(a)(n)
    }

    def addCheck(c: Arounds[O, S, E]) =
      tc.addCheck(a)(c)
  }

  final class Ops3[F[_], R, O, S, E](private val self: Actions[F, R, O, S, E]) extends AnyVal {
    def group(name: NameFn[ROS[R, O, S]]): Actions[F, R, O, S, E] = {
      val i = Group[F, R, O, S, E](Function const Some(self))
      val o = Outer(name, i, Sack.empty)
      o.lift
    }
  }

  private def _timesName[F[_], R, O, S, E](n: Int, name: NameFn[ROS[R, O, S]]) =
    NameFn[ROS[R, O, S]](i => s"${name(i).value} ($n times)")

  private def _timesBody[F[_], R, O, S, E](n: Int, make: (Name => Name) => Actions[F,R,O,S,E]) =
    Sack.Product(
      (1 to n).iterator
        .map(i => make(s => s"[$i/$n] ${s.value}"))
        .toVector)

  private def _times[F[_], R, O, S, E](n: Int, name: NameFn[ROS[R, O, S]], make: (Name => Name) => Actions[F,R,O,S,E]) = {
    val name2 = _timesName(n, name)
    val body = Some(_timesBody(n, make))
    val g = Group[F, R, O, S, E](_ => body)
    Outer(name2, g, Sack.empty)
  }

  @inline implicit class ActionOuterExt[F[_], R, O, S, E](private val self: Outer[F, R, O, S, E]) extends AnyVal {
    @inline def lift: Actions[F, R, O, S, E] =
      Action.liftOuter(self)
  }

  @inline implicit def NameFnRosExt[R, O, S](n: NameFn[ROS[R, O, S]]): NameFnRosExt[R, O, S] = new NameFnRosExt(n.fn)
  class NameFnRosExt[R, O, S](private val _n: Option[ROS[R, O, S]] => Name) extends AnyVal {
    def pmapO[X](f: X => Any Or O): NameFn[ROS[R, X, S]] = NameFn(_n).comap(_.emapO(f).toOption)
    def pmapR[X](f: X => Any Or R): NameFn[ROS[X, O, S]] = NameFn(_n).comap(_.emapR(f).toOption)
  }

  private def failAction[F[_], R, O, S, E](name: Name, err: E)(implicit em: ExecutionModel[F]) =
    Outer(name, Single[F, R, O, S, E](_ => preparedFail(err)), Sack.empty)

  private def someFailActions[F[_], R, O, S, E](name: Name, err: E)(implicit em: ExecutionModel[F]): Some[Actions[F, R, O, S, E]] =
    Some(failAction(name, err).lift)

  private def preparedFail[F[_], O, S, E](err: E)(implicit em: ExecutionModel[F]): Prepared[F, O, S, E] =
    Some(() => em.pure(Left(err)))

  private def tryPrepare[F[_]: ExecutionModel, O, S, E, A](e: E Or A)(f: A => Prepared[F, O, S, E]): Prepared[F, O, S, E] =
    e match {
      case Right(a) => f(a)
      case Left(err) => preparedFail(err)
    }

  trait ImplicitsLowPri {
    implicit def actionsInstanceActionOps: ActionOps[Actions] with ActionOps2[Actions]

    // Scalac doesn't realise shapes are Actions

    import Types.SackE

    implicit def actionSackToActionOps[F[_], R, O, S, E](a: SackE[ROS[R, O, S], Outer[F, R, O, S, E], E]): Ops[Actions, F, R, O, S, E] =
      new Ops[Actions, F, R, O, S, E](a)

    implicit def actionSackToActionOps2[F[_], R, O, S, E](a: SackE[ROS[R, O, S], Outer[F, R, O, S, E], E]): Ops2[Actions, F, R, O, S, E] =
      new Ops2[Actions, F, R, O, S, E](a)
  }

  trait Implicits extends ImplicitsLowPri {

    implicit val actionInnerInstanceActionOps: ActionOps[Inner] =
      new ActionOps[Inner] {

        override def trans[F[_], R, O, S, E, G[_]](x: Inner[F, R, O, S, E])(t: F ~~> G) =
          x match {
            case a: Single [F, R, O, S, E] => a.copy(run = a.run(_).map(f => () => t(f())))
            case a: Group  [F, R, O, S, E] => a.copy(action = a.action(_).map(_ trans t))
            case a: SubTest[F, R, O, S, E] => a.copy(action = a.action trans t)
          }

        override def mapR[F[_], R, O, S, E, X](x: Inner[F, R, O, S, E])(f: X => R) =
          x match {
            case Single (r)    => Single (i => r(i mapR f))
            case Group  (a)    => Group  (i => a(i mapR f).map(_ mapR f))
            case SubTest(a, i) => SubTest(a mapR f, i)
          }

        override def mapOS[F[_], R, O, S, E, X, Y](action: Inner[F, R, O, S, E])(f: X => O, g: Y => S)(h: (Y, S) => Y)(implicit em: ExecutionModel[F]) =
          action match {

            case Single(r) =>
              Single(i =>
                r(i.mapOS(f, g)).map(fn => () =>
                  em.map(fn())(_.map(j => (x: X) => j(f(x)).map(s => h(i.state, s))))))

            case Group(a) =>
              Group(i => a(i.mapOS(f, g)).map(_.mapOS(f, g)(h)))

            case SubTest(a, i) =>
              SubTest(a.mapOS(f, g)(h), i.mapOS(f, g))
          }

        override def mapE[F[_], R, O, S, E, X](action: Inner[F, R, O, S, E])(f: E => X)(implicit em: ExecutionModel[F]) =
          action match {
            case Single (r)    => Single(r(_).map(fn => () => em.map(fn())(_.bimap(f, _.andThen(_ leftMap f)))))
            case Group  (a)    => Group(a(_).map(_ mapE f))
            case SubTest(a, i) => SubTest(a mapE f, i mapE f)
          }

        override def pmapR[F[_], R, O, S, E, X](x: Inner[F, R, O, S, E])(f: X => E Or R)(implicit em: ExecutionModel[F]) =
          x match {
            case Single(run) =>
              Single(ros => tryPrepare(f(ros.ref))(r => run(ros.copy(ref = r))))

            case Group(a) =>
              Group(
                ros => f(ros.ref) match {
                  case Right(r) => a(ros.copy(ref = r)).map(_ pmapR f)
                  case Left(e)  => someFailActions("Action requires correct reference.", e)
                })

            case SubTest(a, i) =>
              SubTest(a pmapR f, i)
          }

        override def pmapO[F[_], R, O, S, E, X](action: Inner[F, R, O, S, E])(f: X => E Or O)(implicit em: ExecutionModel[F]) =
          action match {
            case Single(run) =>
              Single(
                ros => tryPrepare(f(ros.obs))(o =>
                  run(ros.copy(obs = o)).map(fn => () => em.map(fn())(_.map(g => (x: X) => f(x) flatMap g)))))

            case Group(a) =>
              Group(
                ros => f(ros.obs) match {
                  case Right(o) => a(ros.copy(obs = o)).map(_ pmapO f)
                  case Left(err) => someFailActions("Action requires correct observation.", err)
                })

            case SubTest(a, i) =>
              SubTest(a pmapO f, i pmapO f)
          }

        override def modS[F[_], R, O, S, E](x: Inner[F, R, O, S, E])(m: O => S => E Or S)(implicit em: ExecutionModel[F]) =
          x match {
            case Single(run) =>
              Single[F, R, O, S, E](
                run(_).map(f => () => em.map(f())(
                  _.map(g => (o: O) => g(o) flatMap m(o))
                )))

            case Group(a)      => Group(a(_).map(_ modS m))
            case SubTest(a, i) => SubTest(a modS m, i)
          }
      }

    implicit val actionOuterInstanceActionOps: ActionOps[Outer] with ActionOps2[Outer] =
      new ActionOps[Outer] with ActionOps2[Outer] {

        override def trans[F[_], R, O, S, E, G[_]](x: Outer[F, R, O, S, E])(t: F ~~> G) =
          Outer(x.name, x.inner trans t, x.check)

        override def mapR[F[_], R, O, S, E, X](x: Outer[F, R, O, S, E])(f: X => R) =
          Outer(x.name.cmap(_ mapR f), x.inner mapR f, x.check)

        override def mapOS[F[_], R, O, S, E, X, Y](x: Outer[F, R, O, S, E])(f: X => O, g: Y => S)(h: (Y, S) => Y)(implicit em: ExecutionModel[F]) =
          Outer(x.name.cmap(_.mapOS(f, g)), x.inner.mapOS(f, g)(h), x.check.mapOS(f, g))

        override def mapE[F[_], R, O, S, E, X](x: Outer[F, R, O, S, E])(f: E => X)(implicit em: ExecutionModel[F]) =
          Outer(x.name, x.inner mapE f, x.check mapE f)

        override def pmapR[F[_], R, O, S, E, X](x: Outer[F, R, O, S, E])(f: X => E Or R)(implicit em: ExecutionModel[F]) =
          Outer(x.name pmapR f, x.inner pmapR f, x.check)

        override def pmapO[F[_], R, O, S, E, X](x: Outer[F, R, O, S, E])(f: X => E Or O)(implicit em: ExecutionModel[F]) =
          Outer(x.name pmapO f, x.inner pmapO f, x.check pmapO f)

        override def modS[F[_], R, O, S, E](x: Outer[F, R, O, S, E])(m: O => S => E Or S)(implicit em: ExecutionModel[F]) =
          Outer(x.name, x.inner modS m, x.check)

        override def times[F[_], R, O, S, E](a: Outer[F, R, O, S, E])(n: Int) =
          _times(n, a.name, a.nameMod(_).lift)

        override def addCheck[F[_], R, O, S, E](x: Outer[F, R, O, S, E])(c: Arounds[O, S, E]) =
          Outer(x.name, x.inner, x.check & c)
      }

    implicit lazy val actionsInstanceActionOps: ActionOps[Actions] with ActionOps2[Actions] =
      new ActionOps[Actions] with ActionOps2[Actions] {
        import Sack._

        override def trans[F[_], R, O, S, E, G[_]](a: Actions[F, R, O, S, E])(t: F ~~> G) =
          a.rmap(_.map(_ trans t))

        override def mapR[F[_], R, O, S, E, X](x: Actions[F, R, O, S, E])(f: X => R) =
          x.dimap(_ mapR f, _ map (_ mapR f))

        override def mapOS[F[_], R, O, S, E, X, Y](x: Actions[F, R, O, S, E])(f: X => O, g: Y => S)(h: (Y, S) => Y)(implicit em: ExecutionModel[F]) =
          x.dimap(_.mapOS(f, g), _ map (_.mapOS(f, g)(h)))

        override def mapE[F[_], R, O, S, E, X](x: Actions[F, R, O, S, E])(f: E => X)(implicit em: ExecutionModel[F]) =
          x.rmap(_.bimap(_ map f, _ mapE f))

        override def pmapR[F[_], R, O, S, E, X](x: Actions[F, R, O, S, E])(f: X => E Or R)(implicit em: ExecutionModel[F]) =
          x match {
            case Value(v)        => Value(v map(_ pmapR f))
            case Product(ss)     => Product(ss map (_ pmapR f))
            case CoProduct(n, p) =>
              CoProduct(n pmapR f,
                _.emapR(f).fold(e => Sack Value Left(NamedError(n(None), e)), p(_) pmapR f))
          }

        override def pmapO[F[_], R, O, S, E, X](x: Actions[F, R, O, S, E])(f: X => E Or O)(implicit em: ExecutionModel[F]) =
          x match {
            case Value(v)        => Value(v map(_ pmapO f))
            case Product(ss)     => Product(ss map (_ pmapO f))
            case CoProduct(n, p) =>
              CoProduct(n pmapO f,
                _.emapO(f).fold(e => Sack Value Left(NamedError(n(None), e)), p(_) pmapO f))
          }

        override def modS[F[_], R, O, S, E](x: Actions[F, R, O, S, E])(m: O => S => E Or S)(implicit em: ExecutionModel[F]) =
          x.rmap(_.map(_ modS m))

        def groupComposite[F[_], R, O, S, E](ss: Vector[Actions[F, R, O, S, E]]) = {
          val l = ss.length
          val a = Some(Sack.Product(ss))
          val i = Group[F, R, O, S, E](_ => a)
          val o = Outer(s"$l actions.", i, Sack.empty)
          o
        }

        override def times[F[_], R, O, S, E](actions: Actions[F, R, O, S, E])(n: Int) =
          actions match {
            case Value(v)         => Value(v map (_ times n))
            case CoProduct(nf, p) => _times(n, nf, f => CoProduct(nf map f, p)).lift
            case Product(ss)      =>
              if (ss.length == 1)
                ss.head times n
              else {
                groupComposite(ss).times(n).lift
                //_times(n, s"$slen actions.", f => Product(ss map (_ nameMod f))).lift
              }
          }

        override def addCheck[F[_], R, O, S, E](x: Actions[F, R, O, S, E])(c: Arounds[O, S, E]) =
          x match {
            case Product(ss) if ss.length != 1 =>
              groupComposite(ss).addCheck(c).lift
            case _ =>
              x.rmap(_ map (_ addCheck c))
          }
      }

    implicit def toActionOps[A[_[_], _, _, _, _], F[_], R, O, S, E](a: A[F, R, O, S, E])(implicit tc: ActionOps[A]): Ops[A, F, R, O, S, E] =
      new Ops(a)(tc)

    implicit def toActionOps2[A[_[_], _, _, _, _], F[_], R, O, S, E](a: A[F, R, O, S, E])(implicit tc: ActionOps2[A]): Ops2[A, F, R, O, S, E] =
      new Ops2(a)(tc)

    implicit def toActionOps3[F[_], R, O, S, E](a: Actions[F, R, O, S, E]): Ops3[F, R, O, S, E] =
      new Ops3(a)
  }

}
