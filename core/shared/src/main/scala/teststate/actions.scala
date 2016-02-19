package teststate

import Action.{Composite, NonComposite}

sealed trait Action[F[_], Ref, O, S, Err] {
  type This[F[_], R] <: Action[F, R, O, S, Err]

  def trans[G[_]](t: F ~~> G): This[G, Ref]

  def nonCompositeActions: Vector[NonComposite[F, Ref, O, S, Err]]

  def nameMod(f: Name => Name): This[F, Ref]

  def addCheck(c: Check.Around[O, S, Err]): This[F, Ref]

  def when(f: ROS[Ref, O, S] => Boolean): This[F, Ref]

  final def unless(f: ROS[Ref, O, S] => Boolean): This[F, Ref] =
    when(!f(_))

  final def >>(next: Action[F, Ref, O, S, Err]): Composite[F, Ref, O, S, Err] =
    Composite(nonCompositeActions ++ next.nonCompositeActions)

  final def andThen(next: Action[F, Ref, O, S, Err]): Composite[F, Ref, O, S, Err] =
    this >> next

  def cmapRef[R2](f: R2 => Ref): This[F, R2]
}

object Action {

  def empty[F[_], Ref, O, S, E] = Composite[F, Ref, O, S, E](Vector.empty)

  sealed trait NonComposite[F[_], Ref, O, S, Err] extends Action[F, Ref, O, S, Err] {
    override type This[F[_], R] <: NonComposite[F, R, O, S, Err]

    def name: Name.Fn[OS[O, S]]

    def rename(newName: Name.Fn[OS[O, S]]): This[F, Ref]

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

    override type This[F[_], R] = Composite[F, R, O, S, Err]

    def map[G[_], R](f: NonComposite[F, Ref, O, S, Err] => NonComposite[G, R, O, S, Err]): This[G, R] =
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

    def group(name: Name): Group[F, Ref, O, S, Err] =
      Group(_ => name, _ => Some(this), Check.Around.empty)

//    def times(n: Int, name: String) =
//      group(name).times(n)
  }

  final case class Group[F[_], Ref, O, S, Err](name: Name.Fn[OS[O, S]],
                                         action: ROS[Ref, O, S] => Option[Action[F, Ref, O, S, Err]],
                                         check: Check.Around[O, S, Err]) extends NonComposite[F, Ref, O, S, Err] {

    override type This[F[_], R] = Group[F, R, O, S, Err]

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
  }

  final case class Single[F[_], Ref, O, S, Err](name: Name.Fn[OS[O, S]],
                                          run: ROS[Ref, O, S] => Option[() => F[Either[Err, O => S]]],
                                          check: Check.Around[O, S, Err]) extends NonComposite[F, Ref, O, S, Err] {

    override type This[F[_], R] = Single[F, R, O, S, Err]

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
  }
}
