package teststate.dsl

import teststate.typeclass.{Equal, Display}

trait DisplayFailure[-AA, +E] {
  def expectedEqual      [A <: AA](expected: A, actual: A)         (implicit s: Display[A]): E
  def expectedToChange   [A <: AA](a: A)                           (implicit s: Display[A]): E
  def expectedChange     [A <: AA](from: A, expected: A, actual: A)(implicit s: Display[A]): E

  final def expectMaybeEqual[A <: AA](expEqual: Boolean, ex: A, actual: A)(implicit s: Display[A], e: Equal[A]): Option[E] =
    if (expEqual)
      expectEqual(ex, actual)
    else
      expectNotEqual(ex, actual)

  final def expectEqual[A <: AA](expected: A, actual: A)(implicit s: Display[A], e: Equal[A]): Option[E] =
    if (e.equal(expected, actual))
      None
    else
      Some(expectedEqual(expected = expected, actual = actual))

  final def expectNotEqual[A <: AA](unexpected: A, actual: A)(implicit s: Display[A], e: Equal[A]): Option[E] =
    if (e.equal(unexpected, actual))
      Some(expectedToChange(actual))
    else
      None

  final def expectChange[A <: AA](from: A, expected: A, actual: A)(implicit s: Display[A], e: Equal[A]): Option[E] =
    if (e.equal(expected, actual))
      None
    else if (e.equal(from, actual))
      Some(expectedToChange(actual))
    else
      Some(expectedChange(from = from, expected = expected, actual = actual))
}

object DisplayFailure {
  implicit object ToString extends DisplayFailure[Any, String] {
    def expectedEqual[A](expected: A, actual: A)(implicit s: Display[A]) = {
      // No need to display expected because it's displayed as part of the step
      // TODO Seeing as everything is so configurable there should be an option so that reporters can instruct the
      // step name and this failure to display "expected" or not.
      val a = s(actual)
      if (a.contains('\n')) {
        "Got:\n  " + a.replace("\n", "\n  ")
      } else
        s"Got $a."
    }

    def expectedToChange[A](a: A)(implicit s: Display[A]) =
      s"Expected ${s(a)} to change, but it didn't."

    def expectedChange[A](from: A, expected: A, actual: A)(implicit s: Display[A]) =
      s"Got ${s(actual)}, expected ${s(from)} to change into ${s(expected)}."
  }
}
