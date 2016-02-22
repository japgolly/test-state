package teststate

import Action.{Composite, NonComposite}

sealed trait Action[F[_], Ref, O, S, Err] {
  type This[F[_], R, O, S, E] <: Action[F, R, O, S, E]

  def trans[G[_]](t: F ~~> G): This[G, Ref, O, S, Err]

  def nonCompositeActions: Vector[NonComposite[F, Ref, O, S, Err]]

  def nameMod(f: Name => Name): This[F, Ref, O, S, Err]

  def addCheck(c: Check.Around[O, S, Err]): This[F, Ref, O, S, Err]

  def when(f: ROS[Ref, O, S] => Boolean): This[F, Ref, O, S, Err]

  final def unless(f: ROS[Ref, O, S] => Boolean): This[F, Ref, O, S, Err] =
    when(!f(_))

  final def >>(next: Action[F, Ref, O, S, Err]): Composite[F, Ref, O, S, Err] =
    Composite(nonCompositeActions ++ next.nonCompositeActions)

  final def andThen(next: Action[F, Ref, O, S, Err]): Composite[F, Ref, O, S, Err] =
    this >> next

  def cmapRef[R2](f: R2 => Ref): This[F, R2, O, S, Err]

  def pmapRef[R2](f: R2 => Either[Err, Ref])(implicit em: ExecutionModel[F]): This[F, R2, O, S, Err]

  final def cmapO[X](g: X => O)(implicit em: ExecutionModel[F]): This[F, Ref, X, S, Err] =
    mapOS(g, identity, (_, s) => s)

  final def unzoomS[SS](s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]): This[F, Ref, O, SS, Err] =
    mapOS(identity, s, su)

  def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]): This[F, Ref, OO, SS, Err]

  def mapE[E](f: Err => E)(implicit em: ExecutionModel[F]): This[F, Ref, O, S, E]

  def pmapO[OO](f: OO => Either[Err, O])(implicit em: ExecutionModel[F]): This[F, Ref, OO, S, Err]

  final def group(name: Name): Action.Group[F, Ref, O, S, Err] =
    Action.Group(name, _ => Some(this), Check.Around.empty)
}

object Action {

  def empty[F[_], Ref, O, S, E] = Composite[F, Ref, O, S, E](Vector.empty)

//  def apply[F[_], R, O, S, E](as: Action[F, R, O, S, E]*): Action[F, R, O, S, E] =
//    if (as.isEmpty)
//      empty
//    else
//      as.reduce(_ >> _)

  sealed trait NonComposite[F[_], Ref, O, S, Err] extends Action[F, Ref, O, S, Err] {
    override type This[F[_], R, O, S, E] <: NonComposite[F, R, O, S, E]

    def name: NameFn[OS[O, S]]

    def rename(newName: NameFn[OS[O, S]]): This[F, Ref, O, S, Err]

    override final def nameMod(f: Name => Name) =
      rename(name map f)

    final override def nonCompositeActions: Vector[NonComposite[F, Ref, O, S, Err]] =
      vector1(this)

    final def times(n: Int): Group[F, Ref, O, S, Err] =
      Group(NameFn(i => s"${name(i).value} ($n times)"), _ => Some(
        (1 to n).iterator
          .map(i => nameMod(s => s"[$i/$n] ${s.value}"))
          .foldLeft(empty: Action[F, Ref, O, S, Err])(_ >> _)),
        Check.Around.empty)
  }

  final case class Composite[F[_], Ref, O, S, Err](nonCompositeActions: Vector[NonComposite[F, Ref, O, S, Err]])
    extends Action[F, Ref, O, S, Err] {

    override type This[F[_], R, O, S, E] = Composite[F, R, O, S, E]

    def map[f[_], r, o, s, e](f: NonComposite[F, Ref, O, S, Err] => NonComposite[f, r, o, s, e]): This[f, r, o, s, e] =
      Composite(nonCompositeActions map f)

    override def trans[G[_]](t: F ~~> G) =
      map(_ trans t)

    override def nameMod(f: Name => Name) =
      map(_ nameMod f)

    override def addCheck(c: Check.Around[O, S, Err]) =
      map(_ addCheck c)

    override def when(f: ROS[Ref, O, S] => Boolean) =
      map(_ when f)

    override def cmapRef[R2](f: R2 => Ref) =
      map(_ cmapRef f)

    override def pmapRef[R2](f: R2 => Either[Err, Ref])(implicit em: ExecutionModel[F]) =
      map(_ pmapRef f)

    override def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]) =
      map(_.mapOS(o, s, su))

    override def mapE[E](f: Err => E)(implicit em: ExecutionModel[F]) =
      map(_ mapE f)

    override def pmapO[OO](f: OO => Either[Err, O])(implicit em: ExecutionModel[F]) =
      map(_ pmapO f)

//    def times(n: Int, name: String) =
//      group(name).times(n)
  }

  final case class Group[F[_], Ref, O, S, Err](name: NameFn[OS[O, S]],
                                               action: ROS[Ref, O, S] => Option[Action[F, Ref, O, S, Err]],
                                               check: Check.Around[O, S, Err]) extends NonComposite[F, Ref, O, S, Err] {

    override type This[F[_], R, O, S, E] = Group[F, R, O, S, E]

    override def trans[G[_]](t: F ~~> G) =
      copy(action = action(_).map(_ trans t))

    override def rename(newName: NameFn[OS[O, S]]) =
      copy(name = newName)

    override def addCheck(c: Check.Around[O, S, Err]) =
      copy(check = check & c)

    override def when(f: ROS[Ref, O, S] => Boolean) =
      copy(action = i => if (f(i)) action(i) else None)

    override def cmapRef[R2](f: R2 => Ref) =
      copy(action = i => action(i mapR f).map(_ cmapRef f))

    override def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]) =
      Group(
        name.cmap(_.map(o, s)),
        ros => action(ros.mapOS(o, s)).map(_.mapOS(o, s, su)),
        check.cmap(o, s))

    override def mapE[E](f: Err => E)(implicit em: ExecutionModel[F]) =
      Group(
        name,
        action(_) map (_ mapE f),
        check mapE f)

    override def pmapO[OO](f: OO => Either[Err, O])(implicit em: ExecutionModel[F]) =
      Group(
        name.comap(_ mapOe f),
        ros => f(ros.obs) match {
          case Right(o) => action(ros.copyOS(obs = o)).map(_ pmapO f)
          case Left(err) => someFailAction("Action requires correct observation.", err)
        },
        check pmapO f)

    override def pmapRef[R2](f: R2 => Either[Err, Ref])(implicit em: ExecutionModel[F]) =
      Group(
        name,
        ros => f(ros.ref) match {
          case Right(r) => action(ros setRef r).map(_ pmapRef f)
          case Left(err) => someFailAction("Action requires correct reference.", err)
        },
        check)
  }

  private def someFailAction[F[_], Ref, O, S, Err](name: Name, err: Err)(implicit em: ExecutionModel[F]) =
    Some(Single[F, Ref, O, S, Err](
      name,
      _ => preparedFail(err),
      Check.Around.empty))

  type Prepared[F[_], O, S, E] = Option[() => F[Either[E, O => Either[E, S]]]]

  final case class Single[F[_], Ref, O, S, Err](name: NameFn[OS[O, S]],
                                                run: ROS[Ref, O, S] => Prepared[F, O, S, Err],
                                                check: Check.Around[O, S, Err]) extends NonComposite[F, Ref, O, S, Err] {

    override type This[F[_], R, O, S, E] = Single[F, R, O, S, E]

    override def trans[G[_]](t: F ~~> G) =
      copy(run = run(_).map(f => () => t(f())))

    override def rename(newName: NameFn[OS[O, S]]) =
      copy(name = newName)

    override def addCheck(c: Check.Around[O, S, Err]) =
      copy(check = check & c)

    override def when(f: ROS[Ref, O, S] => Boolean) =
      copy(run = i => if (f(i)) run(i) else None)

    override def cmapRef[R2](f: R2 => Ref) =
      copy(run = i => run(i mapR f))

    override def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]) =
      Single[F, Ref, OO, SS, Err](
        name.cmap(_.map(o, s)),
        ros => run(ros.mapOS(o, s)).map(fn => () => em.map(fn())(_.map(f => (oo: OO) => f(o(oo)).map(s => su(ros.state, s))))),
        check.cmap(o, s))

    override def mapE[E](f: Err => E)(implicit em: ExecutionModel[F]) = Single[F, Ref, O, S, E](
      name,
      run(_).map(fn => () => em.map(fn())(_.bimap(f, _.andThen(_ leftMap f)))),
      check mapE f)

    override def pmapO[OO](f: OO => Either[Err, O])(implicit em: ExecutionModel[F]) = Single[F, Ref, OO, S, Err](
      name.comap(_ mapOe f),
      ros => f(ros.obs) match {
        case Right(o) => run(ros.copyOS(obs = o)).map(fn => () => em.map(fn())(_.map(g => (o: OO) => f(o).fmap(g))))
        case Left(err) => preparedFail(err)
      },
      check pmapO f)

    override def pmapRef[R2](f: R2 => Either[Err, Ref])(implicit em: ExecutionModel[F]) = Single[F, R2, O, S, Err](
      name,
      ros => f(ros.ref) match {
        case Right(r) => run(ros setRef r)
        case Left(err) => preparedFail(err)
      },
      check)
  }

  private def preparedFail[F[_], O, S, E](err: E)(implicit em: ExecutionModel[F]): Prepared[F, O, S, E] =
    Some(() => em.pure(Left(err)))

  final case class SubTest[F[_], R, O, S, E](name: NameFn[OS[O, S]],
                                             action: Action[F, R, O, S, E],
                                             invariants: Check[O, S, E]) extends NonComposite[F, R, O, S, E] {
    override type This[F[_], R, O, S, E] = SubTest[F, R, O, S, E]

    override def trans[G[_]](t: F ~~> G) =
      copy(action = action trans t)

    override def rename(newName: NameFn[OS[O, S]]) =
      copy(name = newName)

    override def addCheck(c: Check.Around[O, S, E]) =
      copy(action = action addCheck c)

    override def when(f: ROS[R, O, S] => Boolean) =
      copy(action = action when f)

    override def cmapRef[R2](f: R2 => R) =
      copy(action = action cmapRef f)

    override def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]) =
      SubTest(name.cmap(_.map(o, s)), action.mapOS(o, s, su), invariants.cmap(o, s))

    override def mapE[EE](f: E => EE)(implicit em: ExecutionModel[F]) =
      SubTest(name, action mapE f, invariants mapE f)

    override def pmapO[OO](f: OO => Either[E, O])(implicit em: ExecutionModel[F]) =
      SubTest(name.comap(_ mapOe f), action pmapO f, invariants pmapO f)

    override def pmapRef[R2](f: R2 => Either[E, R])(implicit em: ExecutionModel[F]) =
      copy(action = action pmapRef f)

  }

}
