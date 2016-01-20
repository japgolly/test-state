package teststate

sealed trait Checks[-O1, -S1, -O2, -S2, +Err] {
  def toVector: Vector[Check[O1, S1, O2, S2, Err]]

  final def &[o1 <: O1, s1 <: S1, o2 <: O2, s2 <: S2, e >: Err](c: Checks[o1, s1, o2, s2, e]): Checks.Composite[o1, s1, o2, s2, e] =
    Checks.Composite(toVector ++ c.toVector)
}

object Checks {
  val empty = Composite(Vector.empty)

  case class Composite[-O1, -S1, -O2, -S2, +Err](private val checks: Vector[Check[O1, S1, O2, S2, Err]]) extends Checks[O1, S1, O2, S2, Err] {
    override def toVector: Vector[Check[O1, S1, O2, S2, Err]] =
      checks
  }
}

sealed trait Check[-O1, -S1, -O2, -S2, +Err] extends Checks[O1, S1, O2, S2, Err] {
  type A
  val name: Option[(O1, S1)] => String
  val before: (O1, S1) => Either[Err, A]
  val test: (O2, S2, A) => Option[Err]

  final def aux: Check.Aux[O1, S1, O2, S2, Err, A] =
    this

  override final def toVector: Vector[Check[O1, S1, O2, S2, Err]] =
    vector1(this)
}

object Check {
  type Aux[-O1, -S1, -O2, -S2, +Err, a] = Check[O1, S1, O2, S2, Err] {type A = a}

  def apply[O1, S1, O2, S2, Err, a](_name: Option[(O1, S1)] => String,
                                    _before: (O1, S1) => Either[Err, a],
                                    _test: (O2, S2, a) => Option[Err]): Aux[O1, S1, O2, S2, Err, a] =
    new Check[O1, S1, O2, S2, Err] {
      override type A     = a
      override val name   = _name
      override val before = _before
      override val test   = _test
    }

  private val noBefore = (_: Any, _: Any) => Right(())

  def post[O1, S1, O2, S2, Err](name: Option[(O1, S1)] => String,
                                test: (O2, S2) => Option[Err]): Aux[O1, S1, O2, S2, Err, Unit] =
    apply[O1, S1, O2, S2, Err, Unit](name, noBefore, (s, o, _) => test(s, o))
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
  def toChecks: Checks[O, S, O, S, E]

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
