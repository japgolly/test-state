package teststate

import Action.{Composite, NonComposite}

sealed trait Action[F[_], R, O, S, E] {
  type This[f[_], r, o, s, e] <: Action[f, r, o, s, e]

  def trans[G[_]](t: F ~~> G): This[G, R, O, S, E]

  def nonCompositeActions: Vector[NonComposite[F, R, O, S, E]]

  def nameMod(f: Name => Name): This[F, R, O, S, E]

  def addCheck(c: Check.Around[O, S, E]): This[F, R, O, S, E]

  final def skip: This[F, R, O, S, E] =
    when(_ => false)

  def when(f: ROS[R, O, S] => Boolean): This[F, R, O, S, E]

  final def unless(f: ROS[R, O, S] => Boolean): This[F, R, O, S, E] =
    when(!f(_))

  final def >>(next: Action[F, R, O, S, E]): Composite[F, R, O, S, E] =
    Composite(nonCompositeActions ++ next.nonCompositeActions)

  final def andThen(next: Action[F, R, O, S, E]): Composite[F, R, O, S, E] =
    this >> next

  def cmapR[RR](f: RR => R): This[F, RR, O, S, E]

  def pmapR[RR](f: RR => E Or R)(implicit em: ExecutionModel[F]): This[F, RR, O, S, E]

  final def cmapO[X](g: X => O)(implicit em: ExecutionModel[F]): This[F, R, X, S, E] =
    mapOS(g, identity, (_, s) => s)

  final def unzoomS[SS](s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]): This[F, R, O, SS, E] =
    mapOS(identity, s, su)

  def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]): This[F, R, OO, SS, E]

  def mapE[EE](f: E => EE)(implicit em: ExecutionModel[F]): This[F, R, O, S, EE]

  def pmapO[OO](f: OO => E Or O)(implicit em: ExecutionModel[F]): This[F, R, OO, S, E]

//  final def group(name: Name): Action.Group[F, R, O, S, Err] =
//    Action.Group(name, _ => Some(this), Check.Around.empty)

  // TODO hmmm, Action should just get the updateState methods from DSL
  def modS(f: S => S)(implicit em: ExecutionModel[F]): This[F, R, O, S, E]

  final def withInvariants(is: Check[O, S, E])(implicit em: ExecutionModel[F], r: Recover[E]): TestContent[F, R, O, S, E] =
    Test(this, is)(em, r)

  final def withoutInvariants(implicit em: ExecutionModel[F], r: Recover[E]): TestContent[F, R, O, S, E] =
    Test(this)(em, r)
}

object Action {

  def empty[F[_], Ref, O, S, E] = Composite[F, Ref, O, S, E](Vector.empty)

//  def apply[F[_], R, O, S, E](as: Action[F, R, O, S, E]*): Action[F, R, O, S, E] =
//    if (as.isEmpty)
//      empty
//    else
//      as.reduce(_ >> _)

  private def someFailAction[F[_], R, O, S, E](name: Name, err: E)(implicit em: ExecutionModel[F]) =
    Some(Single[F, R, O, S, E](
      name,
      _ => preparedFail(err),
      Check.Around.empty))

  type Prepared[F[_], O, S, E] = Option[() => F[E Or (O => E Or S)]]

  private def preparedFail[F[_], O, S, E](err: E)(implicit em: ExecutionModel[F]): Prepared[F, O, S, E] =
    Some(() => em.pure(Left(err)))

  private def tryPrepare[F[_]: ExecutionModel, O, S, E, A](e: E Or A)(f: A => Prepared[F, O, S, E]): Prepared[F, O, S, E] =
    e match {
      case Right(a) => f(a)
      case Left(err) => preparedFail(err)
    }

  final case class Composite[F[_], R, O, S, E](nonCompositeActions: Vector[NonComposite[F, R, O, S, E]])
    extends Action[F, R, O, S, E] {

    override type This[f[_], r, o, s, e] = Composite[f, r, o, s, e]

    def map[f[_], r, o, s, e](f: NonComposite[F, R, O, S, E] => NonComposite[f, r, o, s, e]): This[f, r, o, s, e] =
      Composite(nonCompositeActions map f)

    override def trans[G[_]](t: F ~~> G) =
      map(_ trans t)

    override def nameMod(f: Name => Name) =
      map(_ nameMod f)

    override def addCheck(c: Check.Around[O, S, E]) =
      map(_ addCheck c)

    override def when(f: ROS[R, O, S] => Boolean) =
      map(_ when f)

    override def modS(f: S => S)(implicit em: ExecutionModel[F]) =
      map(_ modS f)

    override def cmapR[RR](f: RR => R) =
      map(_ cmapR f)

    override def pmapR[RR](f: RR => E Or R)(implicit em: ExecutionModel[F]) =
      map(_ pmapR f)

    override def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]) =
      map(_.mapOS(o, s, su))

    override def mapE[EE](f: E => EE)(implicit em: ExecutionModel[F]) =
      map(_ mapE f)

    override def pmapO[OO](f: OO => E Or O)(implicit em: ExecutionModel[F]) =
      map(_ pmapO f)

    def group(name: Name): Group[F, R, O, S, E] =
      Group(name, _ => Some(this), Check.Around.empty)

//    def times(n: Int, name: String) =
//      group(name).times(n)
  }

  sealed abstract class NonComposite[F[_], R, O, S, E] extends Action[F, R, O, S, E] {
    override type This[f[_], r, o, s, e] <: NonComposite[f, r, o, s, e]

    def name: NameFn[ROS[R, O, S]]

    def rename(newName: NameFn[ROS[R, O, S]]): This[F, R, O, S, E]

    override final def nameMod(f: Name => Name) =
      rename(name map f)

    final override def nonCompositeActions: Vector[NonComposite[F, R, O, S, E]] =
      vector1(this)

    final def times(n: Int): Group[F, R, O, S, E] =
      Group(NameFn(i => s"${name(i).value} ($n times)"), _ => Some(
        (1 to n).iterator
          .map(i => nameMod(s => s"[$i/$n] ${s.value}"))
          .foldLeft(empty: Action[F, R, O, S, E])(_ >> _)),
        Check.Around.empty)
  }

  final case class Group[F[_], R, O, S, E](name: NameFn[ROS[R, O, S]],
                                           action: ROS[R, O, S] => Option[Action[F, R, O, S, E]],
                                           check: Check.Around[O, S, E]) extends NonComposite[F, R, O, S, E] {

    override type This[f[_], r, o, s, e] = Group[f, r, o, s, e]

    override def trans[G[_]](t: F ~~> G) =
      copy(action = action(_).map(_ trans t))

    override def rename(newName: NameFn[ROS[R, O, S]]) =
      copy(name = newName)

    override def addCheck(c: Check.Around[O, S, E]) =
      copy(check = check & c)

    override def when(f: ROS[R, O, S] => Boolean) =
      copy(action = wrapWithCond(f, action))

    override def modS(f: S => S)(implicit em: ExecutionModel[F]) =
      copy(action = action(_).map(_ modS f))

    override def cmapR[RR](f: RR => R) =
      Group(
        name.cmap(_ mapR f),
        i => action(i mapR f).map(_ cmapR f),
        check)

    override def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]) =
      Group(
        name.cmap(_.mapOS(o, s)),
        ros => action(ros.mapOS(o, s)).map(_.mapOS(o, s, su)),
        check.cmap(o, s))

    override def mapE[EE](f: E => EE)(implicit em: ExecutionModel[F]) =
      Group(
        name,
        action(_) map (_ mapE f),
        check mapE f)

    override def pmapO[OO](f: OO => E Or O)(implicit em: ExecutionModel[F]) =
      Group(
        name.comap(_ mapOe f),
        ros => f(ros.obs) match {
          case Right(o) => action(ros.copyOS(obs = o)).map(_ pmapO f)
          case Left(err) => someFailAction("Action requires correct observation.", err)
        },
        check pmapO f)

    override def pmapR[RR](f: RR => E Or R)(implicit em: ExecutionModel[F]) =
      Group(
        name.comap(_ mapRe f),
        ros => f(ros.ref) match {
          case Right(r) => action(ros setRef r).map(_ pmapR f)
          case Left(err) => someFailAction("Action requires correct reference.", err)
        },
        check)
  }

  final case class Single[F[_], R, O, S, E](name: NameFn[ROS[R, O, S]],
                                            run: ROS[R, O, S] => Prepared[F, O, S, E],
                                            check: Check.Around[O, S, E]) extends NonComposite[F, R, O, S, E] {

    override type This[f[_], r, o, s, e] = Single[f, r, o, s, e]

    override def trans[G[_]](t: F ~~> G) =
      copy(run = run(_).map(f => () => t(f())))

    override def rename(newName: NameFn[ROS[R, O, S]]) =
      copy(name = newName)

    override def addCheck(c: Check.Around[O, S, E]) =
      copy(check = check & c)

    override def when(f: ROS[R, O, S] => Boolean) =
      copy(run = wrapWithCond(f, run))

    override def modS(m: S => S)(implicit em: ExecutionModel[F]) =
      copy(run = run(_).map(f => () => em.map(f())(_.map(_.andThen(_ map m)))))

    override def cmapR[RR](f: RR => R) =
      Single(
        name.cmap(_ mapR f),
        i => run(i mapR f),
        check)

    override def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]) =
      Single[F, R, OO, SS, E](
        name.cmap(_.mapOS(o, s)),
        ros => run(ros.mapOS(o, s)).map(fn => () => em.map(fn())(_.map(f => (oo: OO) => f(o(oo)).map(s => su(ros.state, s))))),
        check.cmap(o, s))

    override def mapE[EE](f: E => EE)(implicit em: ExecutionModel[F]) = Single[F, R, O, S, EE](
      name,
      run(_).map(fn => () => em.map(fn())(_.bimap(f, _.andThen(_ leftMap f)))),
      check mapE f)

    override def pmapO[OO](f: OO => E Or O)(implicit em: ExecutionModel[F]) = Single[F, R, OO, S, E](
      name.comap(_ mapOe f),
      ros => tryPrepare(f(ros.obs))(o =>
        run(ros.copyOS(obs = o)).map(fn => () => em.map(fn())(_.map(g => (o: OO) => f(o) flatMap g)))),
      check pmapO f)

    override def pmapR[RR](f: RR => E Or R)(implicit em: ExecutionModel[F]) = Single[F, RR, O, S, E](
      name.comap(_ mapRe f),
      ros => tryPrepare(f(ros.ref))(r => run(ros setRef r)),
      check)
  }

  final case class SubTest[F[_], R, O, S, E](name: NameFn[ROS[R, O, S]],
                                             action: Action[F, R, O, S, E],
                                             invariants: Check[O, S, E]) extends NonComposite[F, R, O, S, E] {
    override type This[f[_], r, o, s, e] = SubTest[f, r, o, s, e]

    override def trans[G[_]](t: F ~~> G) =
      copy(action = action trans t)

    override def rename(newName: NameFn[ROS[R, O, S]]) =
      copy(name = newName)

    override def addCheck(c: Check.Around[O, S, E]) =
      copy(action = action addCheck c)

    override def when(f: ROS[R, O, S] => Boolean) =
      copy(action = action when f)

    override def cmapR[RR](f: RR => R) =
      SubTest(
        name.cmap(_ mapR f),
        action cmapR f,
        invariants)

    override def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]) =
      SubTest(
        name.cmap(_.mapOS(o, s)),
        action.mapOS(o, s, su),
        invariants.cmap(o, s))

    override def mapE[EE](f: E => EE)(implicit em: ExecutionModel[F]) =
      SubTest(
        name,
        action mapE f,
        invariants mapE f)

    override def pmapO[OO](f: OO => E Or O)(implicit em: ExecutionModel[F]) =
      SubTest(
        name.comap(_ mapOe f),
        action pmapO f,
        invariants pmapO f)

    override def pmapR[RR](f: RR => E Or R)(implicit em: ExecutionModel[F]) =
      SubTest(
        name.comap(_ mapRe f),
        action pmapR f,
        invariants)

    override def modS(f: S => S)(implicit em: ExecutionModel[F]) =
      copy(action = action modS f)
  }

}
