package teststate

sealed trait Checks[S1, O1, S2, O2, Err] {
  def toVector: Vector[Check[S1, O1, S2, O2, Err]]

  final def &(c: Checks[S1, O1, S2, O2, Err]): Checks.Composite[S1, O1, S2, O2, Err] =
    Checks.Composite(toVector ++ c.toVector)
}

object Checks {
  val empty = Composite(Vector.empty)

  case class Composite[S1, O1, S2, O2, Err](private val checks: Vector[Check[S1, O1, S2, O2, Err]]) extends Checks[S1, O1, S2, O2, Err] {
    override def toVector: Vector[Check[S1, O1, S2, O2, Err]] =
      checks
  }
}

sealed trait Check[S1, O1, S2, O2, Err] extends Checks[S1, O1, S2, O2, Err] {
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
  type Aux[S1, O1, S2, O2, Err, a] = Check[S1, O1, S2, O2, Err] {type A = a}

  def apply[S1, O1, S2, O2, Err, a](_name: Option[(S1, O1)] => String,
                                _before: (S1, O1) => Either[Err, a],
                                _test: (S2, O2, a) => Option[Err]): Aux[S1, O1, S2, O2, Err, a] =
    new Check[S1, O1, S2, O2, Err] {
      override type A     = a
      override val name   = _name
      override val before = _before
      override val test   = _test
    }
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