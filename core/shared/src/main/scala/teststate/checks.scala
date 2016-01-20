package teststate

sealed trait Checks[-S1, -O1, -S2, -O2, +Err] {
  def toVector: Vector[Check[S1, O1, S2, O2, Err]]

  final def &[s1 <: S1, o1 <: O1, s2 <: S2, o2 <: O2, e >: Err](c: Checks[s1, o1, s2, o2, e]): Checks.Composite[s1, o1, s2, o2, e] =
    Checks.Composite(toVector ++ c.toVector)
}

object Checks {
  val empty = Composite(Vector.empty)

  case class Composite[-S1, -O1, -S2, -O2, +Err](private val checks: Vector[Check[S1, O1, S2, O2, Err]]) extends Checks[S1, O1, S2, O2, Err] {
    override def toVector: Vector[Check[S1, O1, S2, O2, Err]] =
      checks
  }
}

sealed trait Check[-S1, -O1, -S2, -O2, +Err] extends Checks[S1, O1, S2, O2, Err] {
  type A
  val name: Option[(S1, O1)] => String
  val before: (S1, O1) => Either[Err, A]
  val test: (S2, O2, A) => Option[Err]

  final def aux: Check.Aux[S1, O1, S2, O2, Err, A] =
    this

  override final def toVector: Vector[Check[S1, O1, S2, O2, Err]] =
    vector1(this)
}

object Check {
  type Aux[-S1, -O1, -S2, -O2, +Err, a] = Check[S1, O1, S2, O2, Err] {type A = a}

  def apply[S1, O1, S2, O2, Err, a](_name: Option[(S1, O1)] => String,
                                    _before: (S1, O1) => Either[Err, a],
                                    _test: (S2, O2, a) => Option[Err]): Aux[S1, O1, S2, O2, Err, a] =
    new Check[S1, O1, S2, O2, Err] {
      override type A     = a
      override val name   = _name
      override val before = _before
      override val test   = _test
    }

  private val noBefore = (_: Any, _: Any) => Right(())

  def post[S1, O1, S2, O2, Err](name: Option[(S1, O1)] => String,
                                test: (S2, O2) => Option[Err]): Aux[S1, O1, S2, O2, Err, Unit] =
    apply[S1, O1, S2, O2, Err, Unit](name, noBefore, (s, o, _) => test(s, o))
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



sealed trait Invariants[-S, -O, +E] {
  def toVector: Vector[Invariant[S, O, E]]
  def toChecks: Checks[S, O, S, O, E]

  final def &[s <: S, o <: O, e >: E](c: Invariants[s, o, e]): Invariants.Composite[s, o, e] =
    Invariants.Composite(toVector ++ c.toVector)
}

object Invariants {
  val empty = Composite(Vector.empty)

  case class Composite[-S, -O, +Err](private val invariants: Vector[Invariant[S, O, Err]]) extends Invariants[S, O, Err] {
    override def toVector = invariants
    override def toChecks = Checks.Composite(invariants.map(_.toChecks))
  }
}

case class Invariant[-S, -O, +E](name: Option[(S, O)] => String, test: (S, O) => Option[E]) extends Invariants[S, O, E] {
  override def toVector = vector1(this)
  override def toChecks = Check.post(name, test)
}

object Invariant {
}
