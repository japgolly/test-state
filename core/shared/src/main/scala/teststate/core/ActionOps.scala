package teststate.core

import acyclic.file
import teststate.data._
import teststate.typeclass._
import Action.{Actions => _, _}
import Profunctor.ToOps._
import CoreExports._

// Applies to: Inner, Outer, Sack
trait ActionOps[A[_[_], _, _, _, _]] {

  def trans[F[_], R, O, S, E, G[_]](a: A[F, R, O, S, E])(t: F ~~> G): A[G, R, O, S, E]

  def mapR[F[_], R, O, S, E, X](x: A[F, R, O, S, E])(f: X => R): A[F, X, O, S, E]

  def mapOS[F[_], R, O, S, E, X, Y](a: A[F, R, O, S, E])(f: X => O, g: Y => S)(h: (Y, S) => Y)(implicit em: ExecutionModel[F]): A[F, R, X, Y, E]

  def mapE[F[_], R, O, S, E, X](x: A[F, R, O, S, E])(f: E => X)(implicit em: ExecutionModel[F]): A[F, R, O, S, X]

  def pmapR[F[_], R, O, S, E, X](x: A[F, R, O, S, E])(f: X => E Or R)(implicit em: ExecutionModel[F]): A[F, X, O, S, E]

  def pmapO[F[_], R, O, S, E, X](x: A[F, R, O, S, E])(f: X => E Or O)(implicit em: ExecutionModel[F]): A[F, R, X, S, E]

  /**
    * @param name When defined and the underlying action is empty, a no-op one with the given name will be created.
    *             Otherwise, an empty action will be returned.
    */
  def modS[F[_], R, O, S, E](x: A[F, R, O, S, E])(name: Option[String], f: O => S => E Or S)(implicit em: ExecutionModel[F]): A[F, R, O, S, E]
}

// Applies to: Outer, Sack
trait ActionOps2[A[_[_], _, _, _, _]] {

  def times[F[_], R, O, S, E](a: A[F, R, O, S, E])(n: Int): A[F, R, O, S, E]

  def addCheck[F[_], R, O, S, E](a: A[F, R, O, S, E])(c: Arounds[O, S, E]): A[F, R, O, S, E]

  def topLevelNames[F[_], R, O, S, E](a: A[F, R, O, S, E]): Vector[String]

  def nameTree[F[_], R, O, S, E](a: A[F, R, O, S, E]): VectorTree[String]
}

object ActionOps {

  // Applies to: Inner, Outer, Sack
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

    /**
      * @param name When defined and the underlying action is empty, a no-op one with the given name will be created.
      *             Otherwise, an empty action will be returned.
      */
    private[core] def modS(name: Option[String], f: O => S => E Or S)(implicit em: ExecutionModel[F]) =
      tc.modS(a)(name, f)

    def updateState(f: S => S)(implicit em: ExecutionModel[F]) =
      updateStateAttempt(s => Right(f(s)))

    def updateStateAttempt(f: S => E Or S)(implicit em: ExecutionModel[F]) =
      modS(stateUpdateName, _ => f)

    def updateStateBy(f: OS[O, S] => S)(implicit em: ExecutionModel[F]) =
      updateStateAttemptBy(i => Right(f(i)))

    def updateStateAttemptBy(f: OS[O, S] => E Or S)(implicit em: ExecutionModel[F]) =
      modS(stateUpdateName, o => s => f(OS(o, s)))
  }

  private val stateUpdateName = Some("Update state.")

  // Applies to: Outer, Sack
  final class Ops2[A[_[_], _, _, _, _], F[_], R, O, S, E](private val a: A[F, R, O, S, E])(implicit tc: ActionOps2[A]) {

    def times(n: Int) = {
      require(n >= 0, "n in action.times(n) cannot be negative.")
      tc.times(a)(n)
    }

    def addCheck(c: Arounds[O, S, E]) =
      tc.addCheck(a)(c)

    def topLevelNames: Vector[String] =
      tc.topLevelNames(a)

    def nameTree: VectorTree[String] =
      tc.nameTree(a)

    import teststate.run._

    /** All steps will be marked as skipped. */
    def toReport: Report[Nothing] =
      Report(None, History(topLevelNames.map(n => History.Step(Name.now(n),Result.Skip))), Stats.empty)
  }

  // Applies to: Sack
  final class Ops3[F[_], R, O, S, E](private val self: Actions[F, R, O, S, E]) extends AnyVal {
    def group(name: NameFn[ROS[R, O, S]]): Actions[F, R, O, S, E] = {
      val i = Group.lift(self)
      val o = Outer(name, i, Sack.empty)
      o.lift
    }

    import Sack._

    private def groupIfMultipleBy(name: Vector[Actions[F, R, O, S, E]] => NameFn[ROS[R, O, S]]): Actions[F, R, O, S, E] =
      self match {
        case Product(ss) if ss.length != 1 => _group(name(ss), ss).lift
        case _                             => self
        // Allow 0-actions be grouped for clearer UX.
      }

    def groupIfMultiple(name: NameFn[ROS[R, O, S]]): Actions[F, R, O, S, E] =
      groupIfMultipleBy(_ => name)

    def groupIfMultipleByLen(f: Int => NameFn[ROS[R, O, S]]): Actions[F, R, O, S, E] =
      groupIfMultipleBy(ss => f(ss.length))

    def grouped: Actions[F, R, O, S, E] =
      groupIfMultipleBy(_anonGroupName)
  }

  private def _anonGroupName[F[_], R, O, S, E](ss: Vector[Actions[F, R, O, S, E]]): NameFn[ROS[R, O, S]] = {
    val l = ss.length
    s"$l actions."
  }

  private def _group[F[_], R, O, S, E](name: NameFn[ROS[R, O, S]], ss: Vector[Actions[F, R, O, S, E]]) = {
    val i = Group.lift(Sack.Product(ss))
    val o = Outer(name, i, Sack.empty)
    o
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
    val body = _timesBody(n, make)
    val g = Group.lift(body)
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
            case a: Group  [F, R, O, S, E] => a.copy(actions = a.actions trans t)
            case a: SubTest[F, R, O, S, E] => a.copy(action = a.action trans t)
          }

        override def mapR[F[_], R, O, S, E, X](x: Inner[F, R, O, S, E])(f: X => R) =
          x match {
            case Single (r)    => Single (i => r(i mapR f))
            case Group  (a, c) => Group  (a mapR f, c.mapR(f))
            case SubTest(a, i) => SubTest(a mapR f, i)
          }

        override def mapOS[F[_], R, O, S, E, X, Y](action: Inner[F, R, O, S, E])(f: X => O, g: Y => S)(h: (Y, S) => Y)(implicit em: ExecutionModel[F]) =
          action match {

            case Single(r) =>
              Single(i =>
                r(i.mapOS(f, g)).map(fn => () =>
                  em.map(fn())(_.map(j => (x: X) => j(f(x)).map(s => h(i.state, s))))))

            case Group(a, c) =>
              Group(a.mapOS(f, g)(h), c.mapOS(f, g))

            case SubTest(a, i) =>
              SubTest(a.mapOS(f, g)(h), i.mapOS(f, g))
          }

        override def mapE[F[_], R, O, S, E, X](action: Inner[F, R, O, S, E])(f: E => X)(implicit em: ExecutionModel[F]) =
          action match {
            case Single (r)    => Single(r(_).map(fn => () => em.map(fn())(_.bimap(f, _.andThen(_ leftMap f)))))
            case Group  (a, c) => Group(a mapE f, c mapE f)
            case SubTest(a, i) => SubTest(a mapE f, i mapE f)
          }

        override def pmapR[F[_], R, O, S, E, X](x: Inner[F, R, O, S, E])(f: X => E Or R)(implicit em: ExecutionModel[F]) =
          x match {
            case Single(run) =>
              Single(ros => tryPrepare(f(ros.ref))(r => run(ros.withRef(r))))

            case Group(a, c) =>
              Group(a pmapR f, c pmapR f)

            case SubTest(a, i) =>
              SubTest(a pmapR f, i)
          }

        override def pmapO[F[_], R, O, S, E, X](action: Inner[F, R, O, S, E])(f: X => E Or O)(implicit em: ExecutionModel[F]) =
          action match {
            case Single(run) =>
              Single(
                ros => tryPrepare(f(ros.obs))(o =>
                  run(ros.withObs(o)).map(fn => () => em.map(fn())(_.map(g => (x: X) => f(x) flatMap g)))))

            case Group(a, c) =>
              Group(a pmapO f, c pmapO f)

            case SubTest(a, i) =>
              SubTest(a pmapO f, i pmapO f)
          }

        override def modS[F[_], R, O, S, E](x: Inner[F, R, O, S, E])(name: Option[String], m: O => S => E Or S)(implicit em: ExecutionModel[F]) =
          x match {
            case Single(run) =>
              Single[F, R, O, S, E](
                run(_).map(f => () => em.map(f())(
                  _.map(g => (o: O) => g(o) flatMap m(o))
                )))
            case Group(a, c)   => Group(a.modS(name, m), c.modS(m))
            case SubTest(a, i) => SubTest(a.modS(name, m), i)
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

        override def modS[F[_], R, O, S, E](x: Outer[F, R, O, S, E])(name: Option[String], m: O => S => E Or S)(implicit em: ExecutionModel[F]) =
          Outer(x.name, x.inner.modS(name, m), x.check)

        override def times[F[_], R, O, S, E](a: Outer[F, R, O, S, E])(n: Int) =
          _times(n, a.name, a.nameMod(_).lift)

        override def addCheck[F[_], R, O, S, E](x: Outer[F, R, O, S, E])(c: Arounds[O, S, E]) =
          Outer(x.name, x.inner, x.check & c)

        override def topLevelNames[F[_], R, O, S, E](a: Outer[F, R, O, S, E]): Vector[String] =
          Vector.empty[String] :+ a.name(None).value

        override def nameTree[F[_], R, O, S, E](a: Outer[F, R, O, S, E]): VectorTree[String] = {
          val name = a.name(None).value
          a.inner match {
            case Action.Single(_)      => VectorTree.one(name)
            case Action.Group(a2, _)   => VectorTree.one(name, a2.nameTree)
            case Action.SubTest(a2, _) => VectorTree.one(name, a2.nameTree)
          }
        }
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
          x.rmap(_.bimap(_ map (_ map f), _ mapE f))

        override def pmapR[F[_], R, O, S, E, X](x: Actions[F, R, O, S, E])(f: X => E Or R)(implicit em: ExecutionModel[F]) =
          x match {
            case Value(v)        => Value(v map(_ pmapR f))
            case Product(ss)     => Product(ss map (_ pmapR f))
            case CoProduct(n, p) =>
              CoProduct(n pmapR f,
                _.emapR(f).fold(e => Sack Value Left(NamedError(n(None), Failure NoCause e)), p(_) pmapR f))
          }

        override def pmapO[F[_], R, O, S, E, X](x: Actions[F, R, O, S, E])(f: X => E Or O)(implicit em: ExecutionModel[F]) =
          x match {
            case Value(v)        => Value(v map(_ pmapO f))
            case Product(ss)     => Product(ss map (_ pmapO f))
            case CoProduct(n, p) =>
              CoProduct(n pmapO f,
                _.emapO(f).fold(e => Sack Value Left(NamedError(n(None), Failure NoCause e)), p(_) pmapO f))
          }

        override def modS[F[_], R, O, S, E](actions: Actions[F, R, O, S, E])(name: Option[String], f: O => S => E Or S)(implicit em: ExecutionModel[F]) =
          actions match {
            case Value(v)         => Value(v.map(_.modS(name, f)))
            case CoProduct(nf, p) => CoProduct(nf, ros => p(ros).modS(name, f))
            case Product(ss) =>
              if (ss.isEmpty)
                name match {
                  case Some(n) =>
                    val inner = Action.Single[F, R, O, S, E](ros => Some(() => em.point(Right((o: O) => f(o)(ros.state)))))
                    val action = Action.liftInner(inner)(n)
                    Product(Vector.empty :+ action)
                  case None =>
                    Product(Vector.empty)
                }
              else
                Product(ss.init :+ ss.last.modS(name, f))
          }

        override def times[F[_], R, O, S, E](actions: Actions[F, R, O, S, E])(n: Int) =
          actions match {
            case Value(v)         => Value(v map (_ times n))
            case CoProduct(nf, p) => _times(n, nf, f => CoProduct(nf map f, p)).lift
            case Product(ss)      =>
              if (ss.length == 1)
                ss.head times n
              else
                _group(_anonGroupName(ss), ss).times(n).lift
          }

        override def addCheck[F[_], R, O, S, E](x: Actions[F, R, O, S, E])(c: Arounds[O, S, E]) =
          x match {
            case Product(ss) if ss.length != 1 =>
              _group(_anonGroupName(ss), ss).addCheck(c).lift
            case _ =>
              x.rmap(_ map (_ addCheck c))
          }

        override def topLevelNames[F[_], R, O, S, E](s: Actions[F, R, O, S, E]): Vector[String] =
          s match {
            case Sack.Value(Right(a))    => a.topLevelNames
            case Sack.Value(Left(e))     => Vector.empty[String] :+ e.name.value
            case Sack.CoProduct(name, _) => Vector.empty[String] :+ name(None).value
            case Sack.Product(as)        => as.flatMap(topLevelNames)
          }

        override def nameTree[F[_], R, O, S, E](s: Actions[F, R, O, S, E]): VectorTree[String] =
          s match {
            case Sack.Value(Right(a))    => a.nameTree
            case Sack.Value(Left(e))     => VectorTree.one(e.name.value)
            case Sack.Product(as)        => VectorTree(as.iterator.map(_.nameTree).flatMap(_.elements).toVector)
            case Sack.CoProduct(name, _) => VectorTree.one(name(None).value)
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
