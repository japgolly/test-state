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

    case class Composite[-O, -S, +E](singles: Vector[Single[O, S, E]]) extends Point[O, S, E] {
      override def before = Around.Composite(singles.map(_.before))
      override def after  = Around.Composite(singles.map(_.after ))
    }

    // TODO Should accept OS[O,S]
    case class Single[-O, -S, +E](name: Option[OS[O, S]] => String, test: OS[O, S] => Option[E]) extends Point[O, S, E] {
      override def toString = s"Check.Point.Single(${name(None)})"
      override def singles = vector1(this)
      override def before = Around.before(name, test)
      override def after  = Around.after(name, test)
    }
  }

  // ===================================================================================================================

  sealed abstract class Around[-O, -S, +E] extends Check[O, S, E] {
    def singles: Vector[Around.Single[O, S, E]]

    override final def point  = Point.empty
    override final def around = this

    final def &[o <: O, s <: S, e >: E](c: Around[o, s, e]): Around.Composite[o, s, e] =
      Around.Composite(singles ++ c.singles)
  }

  object Around {
    val empty = Composite(Vector.empty)

    case class Composite[-O, -S, +E](singles: Vector[Single[O, S, E]]) extends Around[O, S, E]

    sealed abstract class Single[-O, -S, +E] extends Around[O, S, E] {
      type A
      val name: Option[OS[O, S]] => String
      val before: OS[O, S] => Either[E, A]
      val test: (OS[O, S], A) => Option[E]

      final def aux: SingleA[O, S, E, A] =
        this

      override final def singles = vector1(this)
    }

    type SingleA[-O, -S, +E, a] = Single[O, S, E] {type A = a}

    def Single[O, S, E, _A](_name: Option[OS[O, S]] => String,
                            _before: OS[O, S] => Either[E, _A],
                            _test: (OS[O, S], _A) => Option[E]): SingleA[O, S, E, _A] =
      new Single[O, S, E] {
        override type A     = _A
        override val name   = _name
        override val before = _before
        override val test   = _test
        override def toString = s"Check.Around.Single(${name(None)})"
      }

    private val rightUnit = Right(())
    private val noBefore = (_: Any) => rightUnit
    private val noAfter = (_: Any, _: Any) => None

    def after[O, S, E](name: Option[OS[O, S]] => String, test: OS[O, S] => Option[E]): SingleA[O, S, E, Unit] =
      Single[O, S, E, Unit](name, noBefore, (os, _) => test(os))

    def before[O, S, E](name: Option[OS[O, S]] => String, test: OS[O, S] => Option[E]): SingleA[O, S, E, Unit] =
      Single[O, S, E, Unit](name, os => test(os).leftOr(()), noAfter)
  }
}
