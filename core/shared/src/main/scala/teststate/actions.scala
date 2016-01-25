package teststate

import Action.{Composite, NonComposite}

sealed trait Action[Ref, O, S, +Err] {
  type This[+E] <: Action[Ref, O, S, E]

  def nonCompositeActions: Vector[NonComposite[Ref, O, S, Err]]

  def nameMod(f: (=> String) => String): This[Err]

  def addCheck[e >: Err](c: Check.Around[O, S, e]): This[e]

  def when(f: ROS[Ref, O, S] => Boolean): This[Err]

  final def unless(f: ROS[Ref, O, S] => Boolean): This[Err] =
    when(!f(_))

  final def >>[e >: Err](next: Action[Ref, O, S, e]): Composite[Ref, O, S, e] =
    Composite(nonCompositeActions ++ next.nonCompositeActions)

  final def andThen[e >: Err](next: Action[Ref, O, S, e]): Composite[Ref, O, S, e] =
    this >> next
}

object Action {

  def empty[Ref, O, S] = Composite[Ref, O, S, Nothing](Vector.empty)

  sealed trait NonComposite[Ref, O, S, +Err] extends Action[Ref, O, S, Err] {
    type This[+E] <: NonComposite[Ref, O, S, E]

    def name: Option[OS[O, S]] => String

    final override def nonCompositeActions: Vector[NonComposite[Ref, O, S, Err]] =
      vector1(this)

    final def times(n: Int): Group[Ref, O, S, Err] =
      Group(i => s"${name(i)} ($n times)", _ => Some(
        (1 to n).iterator
          .map(i => nameMod(s => s"[$i/$n] $s"))
          .foldLeft(empty: Action[Ref, O, S, Err])(_ >> _)),
        Check.Around.empty)
  }

  case class Composite[Ref, O, S, +Err](nonCompositeActions: Vector[NonComposite[Ref, O, S, Err]])
    extends Action[Ref, O, S, Err] {

    override type This[+E] = Composite[Ref, O, S, E]

    def map[E >: Err](f: NonComposite[Ref, O, S, Err] => NonComposite[Ref, O, S, E]): This[E] =
      Composite(nonCompositeActions map f)

    override def nameMod(f: (=> String) => String) =
      map(_ nameMod f)

    override def addCheck[e >: Err](c: Check.Around[O, S, e]) =
      map(_ addCheck c)

    override def when(f: ROS[Ref, O, S] => Boolean) =
      map(_ when f)

    def group(name: String): Group[Ref, O, S, Err] =
      Group(_ => name, _ => Some(this), Check.Around.empty)

//    def times(n: Int, name: String) =
//      group(name).times(n)
  }

  case class Group[Ref, O, S, +Err](name: Option[OS[O, S]] => String,
                                    action: ROS[Ref, O, S] => Option[Action[Ref, O, S, Err]],
                                    check: Check.Around[O, S, Err]) extends NonComposite[Ref, O, S, Err] {

    override type This[+E] = Group[Ref, O, S, E]

    override def nameMod(f: (=> String) => String) =
      copy(name = o => f(name(o)))

    override def addCheck[e >: Err](c: Check.Around[O, S, e]) =
      copy(check = check & c)

    override def when(f: ROS[Ref, O, S] => Boolean) =
      copy(action = i => if (f(i)) action(i) else None)
  }

  case class Single[Ref, O, S, +Err](name: Option[OS[O, S]] => String,
                                     run: ROS[Ref, O, S] => Option[() => Either[Err, O => S]],
                                     check: Check.Around[O, S, Err]) extends NonComposite[Ref, O, S, Err] {

    override type This[+E] = Single[Ref, O, S, E]

    override def nameMod(f: (=> String) => String) =
      copy(name = o => f(name(o)))

    override def addCheck[e >: Err](c: Check.Around[O, S, e]) =
      copy(check = check & c)

    override def when(f: ROS[Ref, O, S] => Boolean) =
      copy(run = i => if (f(i)) run(i) else None)
  }
}
