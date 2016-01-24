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

  def distinctName(name: => String) = s"each $name is unique"

  def distinctI[A](name: => String, input: Any, as: Iterator[A]): EvalL =
    distinct(name, input, as.toList)

  def distinct[A](name: => String, input: Any, as: GenTraversable[A]): EvalL =
    atom(distinctName(name), input, {

      val m = mutable.HashMap.empty[A, Int]
      for (a <- as) {
        val v = m.getOrElse(a, 0) + 1
        m.update(a, v)
      }

      if (m.valuesIterator.forall(_ == 1))
        None
      else
        Some {
          val d = m.iterator
            .filter(_._2 > 1)
            .toList
            .sortBy(_._1.toString)
            .map { case (a, i) => s"$a → $i" }
            .mkString("{", ", ", "}")
          s"Inputs: $as\nDups: $d"
        }
    })

  /**
   * Test that all Cs are on a whitelist.
   */
  def whitelist[B, C](name: => String, input: Any, whitelist: Set[B], testData: => TraversableOnce[C])(implicit ev: C <:< B): EvalL =
      setTest(name, input, true, "Whitelist", whitelist, "Found    ", testData, "Illegal  ")

  /**
   * Test that no Cs are on a blacklist.
   */
  def blacklist[B, C](name: => String, input: Any, blacklist: Set[B], testData: => TraversableOnce[C])(implicit ev: C <:< B): EvalL =
      setTest(name, input, false, "Blacklist", blacklist, "Found    ", testData, "Illegal  ")

  /**
   * Test that all Bs are present in Cs.
   */
  def allPresent[B, C](name: => String, input: Any, required: Set[B], testData: => TraversableOnce[C])(implicit ev: B <:< C): EvalL = {
    val cs = testData.toSet
    atom(name, input, {
      val rs = required.filterNot(cs contains _)
      setMembershipResult(input, "Required", required, "Found   ", testData, "Missing ", rs)
    })
  }

  private[this] def setTest[A, B, C](name: => String, input: Any, expect: Boolean,
                                     bsName: String, bs: Set[B],
                                     csName: String, cs: => TraversableOnce[C],
                                     failureName: String)(implicit ev: C <:< B): EvalL =
    atom(name, input, {
      val rs = cs.foldLeft(Set.empty[C])((q, c) => if (bs.contains(c) == expect) q else q + c)
      setMembershipResult(input, bsName, bs, csName, cs, failureName, rs)
    })

  private[this] def setMembershipResult(input: Any,
                                        asName: String, as: => TraversableOnce[_],
                                        bsName: String, bs: => TraversableOnce[_],
                                        failureName: String, problems: Set[_]): FailureReasonO =
    if (problems.isEmpty)
      None
    else
      Some {
        def fmt(name: String, vs: TraversableOnce[_]) = {
          val x = vs.toIterable
          s"$name: (${x.size}) $x"
        }
        s"$input\n${fmt(asName, as)}\n${fmt(bsName, bs)}\n$failureName: ${fmtSet(problems)}"
      }

  private[this] def fmtSet(s: Set[_]): String =
    s.iterator.map(_.toString).toList.sorted.distinct.mkString("{", ", ", "}")

//  @inline def existance[A](name: String) = new ExistanceB[A](name)
//  final class ExistanceB[A](val name: String) { //extends AnyVal {
//  def apply[B](expect: A => Boolean, expected: A => Set[B], testData: A => Traversable[B]): Prop[A] = {
//    lazy val yes = Prop.allPresent[A](name + " available")(expected, testData)
//    lazy val no = Prop.blacklist[A](name + " not available")(expected, testData)
//    Prop.test[A](name, expect).ifelse(yes, no)
//  }
//  }

   */

  def distinct[A](as: Traversable[A])(implicit s: Show[A]): Option[FailedDistinct[A]] = {

    // TODO only works when A has universal equality and appropriate hashcodes
    val m = mutable.HashMap.empty[A, Int]
    for (a <- as) {
      val v = m.getOrElse(a, 0) + 1
      m.update(a, v)
    }

    if (m.valuesIterator.forall(_ == 1))
      None
    else
      Some {
        val d = m.iterator
          .filter(_._2 > 1)
          .toList
          .sortBy(_._1.toString)
        FailedDistinct(as, d)
      }
  }

  case class FailedDistinct[A](input: Traversable[A], dups: List[(A, Int)])(implicit s: Show[A]) {
    def dupsToString = dups.iterator
      .map { case (a, i) => s"${s(a)} → $i" }
      .mkString("{", ", ", "}")

    override def toString =
      s"Input: $input\nDups: $dupsToString"
  }


  implicit def formatFailedDistinct[A](f: FailedDistinct[A]) = f.toString

}
