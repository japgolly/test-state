package teststate

import scala.collection.{GenTraversable, mutable}

object CollAssert {

  // exists

  /*

  def forall[F[_]: Foldable, B](input: Any, fb: F[B])(each: B => EvalL): EvalL = {
    val es = fb.foldLeft(List.empty[Eval])((q, b) => run(each(b)) :: q)
    val ho = es.headOption
    val n  = Need(ho.fold("∅")(e => s"∀{${e.name.value}}"))
    val i  = Input(input)
    val r  = es.filter(_.failure) match {
      case Nil =>
        Eval.success(n, i)
      case fs@(_ :: _) =>
        val causes = fs.foldLeft(Eval.root)((q, e) => q.add(e.name.value, List(e)))
        Eval(n, i, causes)
    }
    r.liftL
  }
   */

  private def formatSet(s: TraversableOnce[_]): String =
    s.mkString("", ", ", ".")

  def distinct[A](as: TraversableOnce[A])(implicit s: Show[A]): Option[FailedDistinct[A]] = {

    // TODO only works when A has universal equality and appropriate hashcodes
    val m = mutable.HashMap.empty[A, Int]
    for (a <- as) {
      val v = m.getOrElse(a, 0) + 1
      m.update(a, v)
    }

    if (m.valuesIterator.forall(_ == 1))
      None
    else
      Some(FailedDistinct(
        m.iterator
          .filter(_._2 > 1)
          .map(x => (x, s(x._1)))
          .toList
          .sortBy(_._2)
          .map(_._1)
      ))
  }

  case class FailedDistinct[+A](dups: List[(A, Int)])(implicit s: Show[A]) extends HasErrorString {
    def dupsToString = formatSet(dups.iterator.map { case (a, i) => s"${s(a)} → $i" })
    override def errorString = s"Dups: $dupsToString"
  }

  // -------------------------------------------------------------------------------------------------

  /**
    * Test that all Bs are present.
    */
  def containsAll[B, A](required: Set[B], actual: TraversableOnce[A])(implicit ev: B <:< A, sb: Show[B]) = {
    val as = actual.toSet
    val missing = required.iterator.filterNot(as contains _)
    if (missing.isEmpty)
      None
    else
      Some(FailedContainsAll(missing.toSet, required))
  }

  case class FailedContainsAll[B](missing: Set[B], required: Set[B])(implicit s: Show[B]) extends HasErrorString {
    def missingToString = formatSet(missing.iterator.map(s(_)))
    override def errorString = s"Missing: $missingToString"
  }

  // -------------------------------------------------------------------------------------------------

  /**
   * Test that all As are on a whitelist.
   */
  def containsOnly[B, A](whitelist: Set[B], actual: TraversableOnce[A])(implicit ev: A <:< B, sa: Show[A]) = {
    var bad = Vector.empty[A]
    for (a <- actual)
      if (!whitelist.contains(a))
        bad :+= a
    if (bad.isEmpty)
      None
    else
      Some(FailedContainsOnly(bad))
  }

  case class FailedContainsOnly[+A](bad: Vector[A])(implicit s: Show[A]) extends HasErrorString {
    def badToString = formatSet(bad.iterator.map(s(_)))
    override def errorString = s"Data not on whitelist: $badToString"
  }

  // -------------------------------------------------------------------------------------------------

  /**
   * Test that no As are on a blacklist.
   */
  def containsNone[B, A](blacklist: Set[B], actual: TraversableOnce[A])(implicit ev: A <:< B, sa: Show[A]) = {
    var bad = Vector.empty[A]
    for (a <- actual)
      if (blacklist.contains(a))
        bad :+= a
    if (bad.isEmpty)
      None
    else
      Some(FailedContainsNone(bad))
  }

  case class FailedContainsNone[+A](bad: Vector[A])(implicit s: Show[A]) extends HasErrorString {
    def badToString = formatSet(bad.iterator.map(s(_)))
    override def errorString = s"Data on blacklist: $badToString"
  }

  // -------------------------------------------------------------------------------------------------

  def existence[A: Show](expect: Boolean, expected: Set[A], actual: TraversableOnce[A]): Option[Either[FailedContainsNone[A], FailedContainsAll[A]]] =
    if (expect)
      containsAll(expected, actual).map(Right(_))
    else
      containsNone(expected, actual).map(Left(_))

  // -------------------------------------------------------------------------------------------------

  def equalIgnoringOrder[A: Show](expect: TraversableOnce[A], actual: TraversableOnce[A]): Option[FailedEqualIgnoringOrder[A]] = {
    val m = mutable.HashMap.empty[A, Int]
    def go(as: TraversableOnce[A], n: Int): Unit =
      for (a <- as)
        m.update(a, m.get(a).fold(n)(_ + n))
    go(expect, -1)
    go(actual, 1)
    if (m.valuesIterator.forall(_ == 0))
      None
    else
      Some {
        val missing = Vector.newBuilder[A]
        val excess  = Vector.newBuilder[A]
        for {
          (a, n) <- m if n != 0
          target = if (n < 0) missing else excess
          _ <- 1 to n.abs
        } target += a
        FailedEqualIgnoringOrder(missing.result(), excess.result())
      }
  }

  case class FailedEqualIgnoringOrder[+A](missing: Vector[A], excess: Vector[A])(implicit s: Show[A]) extends HasErrorString {
    private def fmt[AA >: A](name: String, as: Vector[AA])(implicit s: Show[AA]): Option[String] =
      if (as.isEmpty) None else Some(name + ": " +formatSet(as.iterator.map(s(_))))
    override def errorString =
      (fmt("Missing", missing).toList ::: fmt("Excess", excess).toList).mkString(", ")
  }
}
