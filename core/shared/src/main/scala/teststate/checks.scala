package teststate

sealed abstract class Check[-O, -S, +E] {

  final def &[o <: O, s <: S, e >: E](c: Check[o, s, e]): Check.Composite[o, s, e] =
    Check.Composite(point & c.point, around & c.around)

  def point : Check.Point[O, S, E]
  def around: Check.Around[O, S, E]
}

object Check {

  val empty = Composite(Point.empty, Around.empty)

  case class Composite[-O, -S, +E](point: Point[O, S, E], around: Around[O, S, E]) extends Check[O, S, E]

  // ===================================================================================================================

  sealed abstract class Point[-O, -S, +E] extends Check[O, S, E] {
    def singles: Vector[Point.Single[O, S, E]]
    def before: Around[O, S, E]
    def after : Around[O, S, E]

    final def beforeAndAfter: Around.Composite[O, S, E] =
      before & after

    override final def point  = this
    override final def around = Around.empty

    final def &[o <: O, s <: S, e >: E](c: Point[o, s, e]): Point.Composite[o, s, e] =
      Point.Composite(singles ++ c.singles)
  }

  object Point {
    val empty = Composite(Vector.empty)

    final case class Composite[-O, -S, +E](singles: Vector[Single[O, S, E]]) extends Point[O, S, E] {
      override def before = Around.empty.copy(befores = singles.map(_.before))
      override def after  = Around.empty.copy(afters = singles.map(_.after))
    }

    final case class Single[-O, -S, +E](name: Name.Fn[OS[O, S]], test: OS[O, S] => Option[E]) extends Point[O, S, E] {
      override def toString = s"Check.Point.Single(${name(None)})"
      override def singles = vector1(this)
      override def before = Around.Before(this)
      override def after  = Around.After(this)
      def rename[o <: O, s <: S, e >: E](newName: Name.Fn[OS[o, s]]): Single[o, s, e] =
        copy(newName)
    }
  }

  // ===================================================================================================================

  sealed abstract class Around[-O, -S, +E] extends Check[O, S, E] {
    def befores: Vector[Around.Before[O, S, E]]
    def dunnos: Vector[Around.Dunno[O, S, E]]
    def afters: Vector[Around.After[O, S, E]]

    override final def point  = Point.empty
    override final def around = this

    final def &[o <: O, s <: S, e >: E](c: Around[o, s, e]): Around.Composite[o, s, e] =
      Around.Composite(
        befores ++ c.befores,
        dunnos ++ c.dunnos,
        afters ++ c.afters)
  }

  object Around {
    val empty = Composite(Vector.empty, Vector.empty, Vector.empty)

    final case class Composite[-O, -S, +E](befores: Vector[Before[O, S, E]],
                                     dunnos: Vector[Dunno[O, S, E]],
                                     afters: Vector[After[O, S, E]]) extends Around[O, S, E]

    sealed abstract class Single[-O, -S, +E] extends Around[O, S, E] {
      type This[-o, -s, +e] <: Single[o, s, e]

      override def befores: Vector[Before[O, S, E]] = Vector.empty
      override def dunnos: Vector[Dunno[O, S, E]] = Vector.empty
      override def afters: Vector[After[O, S, E]] = Vector.empty

      def rename[o <: O, s <: S, e >: E](newName: Name.Fn[OS[o, s]]): This[o, s, e]
    }

    sealed abstract class Dunno[-O, -S, +E] extends Single[O, S, E] {
      type A
      val name: Name.Fn[OS[O, S]]
      val before: OS[O, S] => A
      val test: (OS[O, S], A) => Option[E]

      final def aux: DunnoA[O, S, E, A] = this
      final override type This[-o, -s, +e] = DunnoA[o, s, e, A]
      final override def dunnos = vector1(this)
    }

    type DunnoA[-O, -S, +E, a] = Dunno[O, S, E] {type A = a}

    def Dunno[O, S, E, _A](_name: Name.Fn[OS[O, S]],
                            _before: OS[O, S] => _A,
                            _test: (OS[O, S], _A) => Option[E]): DunnoA[O, S, E, _A] =
      new Dunno[O, S, E] {
        override type A     = _A
        override val name   = _name
        override val before = _before
        override val test   = _test
        override def toString = s"Check.Around.Single(${name(None)})"
        override def rename[o <: O, s <: S, e >: E](newName: Name.Fn[OS[o, s]]) =
          Dunno(newName, before, test)
      }

    final case class Before[-O, -S, +E](check: Point.Single[O, S, E]) extends Single[O, S, E] {
      override type This[-o, -s, +e] = Before[o, s, e]
      override def befores = vector1(this)
      override def rename[o <: O, s <: S, e >: E](newName: Name.Fn[OS[o, s]]) =
        copy(check rename newName)
    }

    final case class After[-O, -S, +E](check: Point.Single[O, S, E]) extends Single[O, S, E] {
      override type This[-o, -s, +e] = After[o, s, e]
      override def afters = vector1(this)
      override def rename[o <: O, s <: S, e >: E](newName: Name.Fn[OS[o, s]]) =
        copy(check rename newName)
    }
  }
}
