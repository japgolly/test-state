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

    override final def point  = this
    override final def around = Around.empty

    final def &[o <: O, s <: S, e >: E](c: Point[o, s, e]): Point.Composite[o, s, e] =
      Point.Composite(singles ++ c.singles)
  }

  object Point {
    val empty = Composite(Vector.empty)

    case class Composite[-O, -S, +E](singles: Vector[Single[O, S, E]]) extends Point[O, S, E]

    case class Single[-O, -S, +E](name: Option[(O, S)] => String, test: (O, S) => Option[E]) extends Point[O, S, E] {
      override def toString = s"Check.Point.Single(${name(None)})"
      override final def singles = vector1(this)
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
      val name: Option[(O, S)] => String
      val before: (O, S) => Either[E, A]
      val test: (O, S, A) => Option[E]

      final def aux: SingleA[O, S, E, A] =
        this

      override final def singles = vector1(this)
    }

    type SingleA[-O, -S, +E, a] = Single[O, S, E] {type A = a}

    def Single[O, S, E, _A](_name: Option[(O, S)] => String,
                            _before: (O, S) => Either[E, _A],
                            _test: (O, S, _A) => Option[E]): SingleA[O, S, E, _A] =
      new Single[O, S, E] {
        override type A     = _A
        override val name   = _name
        override val before = _before
        override val test   = _test
        override def toString = s"Check.Around.Single(${name(None)})"
      }

//    private val noBefore = (_: Any, _: Any) => Right(())
//
//    def post[O, S, E](name: Option[(O, S)] => String,
//                        test: (O, S) => Option[E]): Aux[O, S, E, Unit] =
//      apply[O, S, E, Unit](name, noBefore, (s, o, _) => test(s, o))

  }
}
