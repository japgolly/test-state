package teststate

// pre/post
// compose
// before/after
// around

/*
sealed trait BeforeOrAfter
case object Before extends BeforeOrAfter
case object After extends BeforeOrAfter
*/

sealed trait Checks[State, Obs, Err] {
  def toVector: Vector[Check[State, Obs, Err]]

  final def &(c: Checks[State, Obs, Err]): Check.Composite[State, Obs, Err] =
    Check.Composite(toVector ++ c.toVector)
}

/*
sealed trait Check[State, Obs, Err] extends Checks[State, Obs, Err] {
  val name: Option[(State, Obs)] => String

  override final def toVector: Vector[Check[State, Obs, Err]] =
    Vector.empty[Check[State, Obs, Err]] :+ this
}
*/

sealed trait Check[State, Obs, Err] extends Checks[State, Obs, Err] {
  type A
  val name: Option[(State, Obs)] => String
  val before: (State, Obs) => Either[Err, A]
  val test: (State, Obs, A) => Option[Err]

  override final def toVector: Vector[Check[State, Obs, Err]] =
    Vector.empty[Check[State, Obs, Err]] :+ this
}

object Check {
  val empty = Composite(Vector.empty)

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

  /*
  case class Once[State, Obs, Err, A](name: Option[(State, Obs)] => String,
                                      when: BeforeOrAfter,
                                      test: (State, Obs) => Option[Err]) extends Check[State, Obs, Err]

  case class Around[State, Obs, Err, A](name: Option[(State, Obs)] => String,
                                        before: (State, Obs) => A,
                                        test: (State, Obs, A) => Option[Err]) extends Check[State, Obs, Err]
                                        */

  /*
  case class Composite[State, Obs, Err](checks: Vector[Checks[State, Obs, Err]]) extends Checks[State, Obs, Err] {
    def flat: Vector[Check[State, Obs, Err]] = {
      val b = Vector.newBuilder[Check[State, Obs, Err]]

      def go(c: Checks[State, Obs, Err]): Unit =
        c match {
          case n: Check[State, Obs, Err] => b += n
          case c: Composite[State, Obs, Err] => all(c)
        }

      def all(c: Composite[State, Obs, Err]): Unit =
        c.checks foreach go

      all(this)

      b.result()
    }
  }
  */

  case class Composite[State, Obs, Err](checks: Vector[Check[State, Obs, Err]]) extends Checks[State, Obs, Err] {
    override def toVector: Vector[Check[State, Obs, Err]] =
      checks
  }

}

/*
trait CheckAroundBuilder[State, Obs, Err, A] {

  read   : (State, Obs) => A

  compare: (A, A) => Boolean
  assertChange
  assertNoChange
  assert(before, after)

  assertChangeBy

  // a.focus(locked_?) .assertBefore(true).assertAfter(false)
}
 */