package teststate

sealed trait Checks[-O, -S, +Err] {
  def toVector: Vector[Check[O, S, Err]]

  final def &[o <: O, s <: S, e >: Err](c: Checks[o, s, e]): Checks.Composite[o, s, e] =
    Checks.Composite(toVector ++ c.toVector)
}

object Checks {
  val empty = Composite(Vector.empty)

  case class Composite[-O, -S, +Err](private val checks: Vector[Check[O, S, Err]]) extends Checks[O, S, Err] {
    override def toVector: Vector[Check[O, S, Err]] =
      checks
  }
}

sealed trait Check[-O, -S, +Err] extends Checks[O, S, Err] {
  type A
  val name: Option[(O, S)] => String
  val before: (O, S) => Either[Err, A]
  val test: (O, S, A) => Option[Err]

  final def aux: Check.Aux[O, S, Err, A] =
    this

  override final def toVector: Vector[Check[O, S, Err]] =
    vector1(this)
}

object Check {
  type Aux[-O, -S, +Err, a] = Check[O, S, Err] {type A = a}

  def apply[O, S, Err, a](_name: Option[(O, S)] => String,
                          _before: (O, S) => Either[Err, a],
                          _test: (O, S, a) => Option[Err]): Aux[O, S, Err, a] =
    new Check[O, S, Err] {
      override type A     = a
      override val name   = _name
      override val before = _before
      override val test   = _test
    }

  private val noBefore = (_: Any, _: Any) => Right(())

  def post[O, S, Err](name: Option[(O, S)] => String,
                                test: (O, S) => Option[Err]): Aux[O, S, Err, Unit] =
    apply[O, S, Err, Unit](name, noBefore, (s, o, _) => test(s, o))
}

/*
  compare: (A, A) => Boolean
  assertChange
  assertNoChange
  assert(before, after)

  assertChangeBy

  // a.focus(locked_?) .assertBefore(true).assertAfter(false)
}
 */



sealed trait Invariants[-O, -S, +E] {
  def toVector: Vector[Invariant[O, S, E]]
  def toChecks: Checks[O, S, E]

  final def &[o <: O, s <: S, e >: E](c: Invariants[o, s, e]): Invariants.Composite[o, s, e] =
    Invariants.Composite(toVector ++ c.toVector)
}

object Invariants {
  val empty = Composite(Vector.empty)

  case class Composite[-O, -S, +E](private val invariants: Vector[Invariant[O, S, E]]) extends Invariants[O, S, E] {
    override def toVector = invariants
    override def toChecks = Checks.Composite(invariants.map(_.toChecks))
  }
}

case class Invariant[-O, -S, +E](name: Option[(O, S)] => String, test: (O, S) => Option[E]) extends Invariants[O, S, E] {
  override def toVector = vector1(this)
  override def toChecks = Check.post(name, test)
}

object Invariant {
}
