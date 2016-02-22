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

  final def cmapO[X](g: X => O)(implicit em: ExecutionModel[F]): This[F, Ref, X, S, Err] =
    mapOS(g, identity, (_, s) => s)

  final def unzoomS[SS](s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]): This[F, Ref, O, SS, Err] =
    mapOS(identity, s, su)

  def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]): This[F, Ref, OO, SS, Err]

  def mapE[E](f: Err => E)(implicit em: ExecutionModel[F]): This[F, Ref, O, S, E]
}

object Action {

  def empty[F[_], Ref, O, S, E] = Composite[F, Ref, O, S, E](Vector.empty)

  sealed trait NonComposite[F[_], Ref, O, S, Err] extends Action[F, Ref, O, S, Err] {
    override type This[F[_], R, O, S, E] <: NonComposite[F, R, O, S, E]

    def name: Name.Fn[OS[O, S]]

    def rename(newName: Name.Fn[OS[O, S]]): This[F, Ref, O, S, Err]

    override final def nameMod(f: Name => Name) =
      rename(f compose name)

    final override def nonCompositeActions: Vector[NonComposite[F, Ref, O, S, Err]] =
      vector1(this)

    final def times(n: Int): Group[F, Ref, O, S, Err] =
      Group(i => s"${name(i)} ($n times)", _ => Some(
        (1 to n).iterator
          .map(i => nameMod(s => s"[$i/$n] $s"))
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

    override def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]) =
      map(_.mapOS(o, s, su))

    override def mapE[E](f: Err => E)(implicit em: ExecutionModel[F]) =
      map(_ mapE f)

    def group(name: Name): Group[F, Ref, O, S, Err] =
      Group(_ => name, _ => Some(this), Check.Around.empty)

//    def times(n: Int, name: String) =
//      group(name).times(n)
  }

  final case class Group[F[_], Ref, O, S, Err](name: Name.Fn[OS[O, S]],
                                         action: ROS[Ref, O, S] => Option[Action[F, Ref, O, S, Err]],
                                         check: Check.Around[O, S, Err]) extends NonComposite[F, Ref, O, S, Err] {

    override type This[F[_], R, O, S, E] = Group[F, R, O, S, E]

    override def trans[G[_]](t: F ~~> G) =
      copy(action = action(_).map(_ trans t))

    override def rename(newName: Name.Fn[OS[O, S]]) =
      copy(name = newName)

    override def addCheck(c: Check.Around[O, S, Err]) =
      copy(check = check & c)

    override def when(f: ROS[Ref, O, S] => Boolean) =
      copy(action = i => if (f(i)) action(i) else None)

    override def cmapRef[R2](f: R2 => Ref) =
      copy(action = i => action(i mapR f).map(_ cmapRef f))

    override def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]) =
      Group(
        Name.cmapFn(name)(_.map(o, s)),
        ros => action(ros.mapOS(o, s)).map(_.mapOS(o, s, su)),
        check.cmap(o, s))

    override def mapE[E](f: Err => E)(implicit em: ExecutionModel[F]) =
      Group(
        name,
        action(_) map (_ mapE f),
        check mapE f)
  }

  final case class Single[F[_], Ref, O, S, Err](name: Name.Fn[OS[O, S]],
                                          run: ROS[Ref, O, S] => Option[() => F[Either[Err, O => S]]],
                                          check: Check.Around[O, S, Err]) extends NonComposite[F, Ref, O, S, Err] {

    override type This[F[_], R, O, S, E] = Single[F, R, O, S, E]

    override def trans[G[_]](t: F ~~> G) =
      copy(run = run(_).map(f => () => t(f())))

    override def rename(newName: Name.Fn[OS[O, S]]) =
      copy(name = newName)

    override def addCheck(c: Check.Around[O, S, Err]) =
      copy(check = check & c)

    override def when(f: ROS[Ref, O, S] => Boolean) =
      copy(run = i => if (f(i)) run(i) else None)

    override def cmapRef[R2](f: R2 => Ref) =
      copy(run = i => run(i mapR f))

    override def mapOS[OO, SS](o: OO => O, s: SS => S, su: (SS, S) => SS)(implicit em: ExecutionModel[F]) =
      Single[F, Ref, OO, SS, Err](
        Name.cmapFn(name)(_.map(o, s)),
        ros => run(ros.mapOS(o, s)).map(fn => () => em.map(fn())(_.map(os => (oo: OO) => su(ros.state, os(o(oo)))))),
        check.cmap(o, s))

    override def mapE[E](f: Err => E)(implicit em: ExecutionModel[F]) = Single[F, Ref, O, S, E](
      name,
      run(_).map(fn => () => em.map(fn())(_ leftMap f)),
      check mapE f)
  }
}
