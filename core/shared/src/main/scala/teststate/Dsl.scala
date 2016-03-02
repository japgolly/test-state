package teststate

case class Equal[A](equal: (A, A) => Boolean)
object Equal {

  def byUnivEq[A]: Equal[A] = Equal(_ == _)

  implicit val equalUnit   : Equal[Unit] = byUnivEq
  implicit val equalChar   : Equal[Char] = byUnivEq
  implicit val equalString : Equal[String] = byUnivEq
  implicit val equalBoolean: Equal[Boolean] = byUnivEq
  implicit val equalInt    : Equal[Int] = byUnivEq
  implicit val equalLong   : Equal[Long] = byUnivEq

  implicit def equalOption[A](implicit e: Equal[A]): Equal[Option[A]] =
    Equal((a, b) => a match {
      case None => b.isEmpty
      case Some(x) => b.exists(e.equal(x, _))
    })
}

trait SomethingFailures[-AA, +E] {
  def expectedEqual      [A <: AA](expected: A, actual: A)         (implicit s: Show[A]): E
  def expectedToChange   [A <: AA](a: A)                           (implicit s: Show[A]): E
  def expectedChange     [A <: AA](from: A, expected: A, actual: A)(implicit s: Show[A]): E

  final def expectMaybeEqual[A <: AA](expEqual: Boolean, ex: A, actual: A)(implicit s: Show[A], e: Equal[A]): Option[E] =
    if (expEqual)
      expectEqual(ex, actual)
    else
      expectNotEqual(ex, actual)

  final def expectEqual[A <: AA](expected: A, actual: A)(implicit s: Show[A], e: Equal[A]): Option[E] =
    if (e.equal(expected, actual))
      None
    else
      Some(expectedEqual(expected = expected, actual = actual))

  final def expectNotEqual[A <: AA](unexpected: A, actual: A)(implicit s: Show[A], e: Equal[A]): Option[E] =
    if (e.equal(unexpected, actual))
      Some(expectedToChange(actual))
    else
      None

  final def expectChange[A <: AA](from: A, expected: A, actual: A)(implicit s: Show[A], e: Equal[A]): Option[E] =
    if (e.equal(expected, actual))
      None
    else if (e.equal(from, actual))
      Some(expectedToChange(actual))
    else
      Some(expectedChange(from = from, expected = expected, actual = actual))
}
object SomethingFailures {
  implicit object ToString extends SomethingFailures[Any, String] {
    def expectedEqual      [A](expected: A, actual: A)         (implicit s: Show[A]) = s"Expected ${s(expected)}, not ${s(actual)}."
    def expectedToChange   [A](a: A)                           (implicit s: Show[A]) = s"Expected ${s(a)} to change, but it didn't."
    def expectedChange     [A](from: A, expected: A, actual: A)(implicit s: Show[A]) = s"Expected ${s(from)} to change into ${s(expected)}, not ${s(actual)}."
  }
}
