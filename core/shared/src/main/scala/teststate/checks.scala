package teststate

sealed trait Checks[State, Obs, Err] {
  def toVector: Vector[Check[State, Obs, Err]]

  final def &(c: Checks[State, Obs, Err]): Checks.Composite[State, Obs, Err] =
    Checks.Composite(toVector ++ c.toVector)
}

object Checks {
  val empty = Composite(Vector.empty)

  case class Composite[State, Obs, Err](checks: Vector[Check[State, Obs, Err]]) extends Checks[State, Obs, Err] {
    override def toVector: Vector[Check[State, Obs, Err]] =
      checks
  }
}

sealed trait Check[State, Obs, Err] extends Checks[State, Obs, Err] {
  type A
  val name: Option[(State, Obs)] => String
  val before: (State, Obs) => Either[Err, A]
  val test: (State, Obs, A) => Option[Err]

  override final def toVector: Vector[Check[State, Obs, Err]] =
    Vector.empty[Check[State, Obs, Err]] :+ this
}

object Check {
  type Aux[State, Obs, Err, a] = Check[State, Obs, Err] {type A = a}

  def apply[State, Obs, Err, a](_name: Option[(State, Obs)] => String,
                                _before: (State, Obs) => Either[Err, a],
                                _test: (State, Obs, a) => Option[Err]): Aux[State, Obs, Err, a] =
    new Check[State, Obs, Err] {
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