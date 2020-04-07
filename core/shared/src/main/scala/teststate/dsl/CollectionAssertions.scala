package teststate.dsl

import acyclic.file
import japgolly.univeq.UnivEq
import scala.annotation.tailrec
import scala.collection.compat._
import scala.collection.mutable
import teststate.data.Name.Implicits._
import teststate.data.{Name, NameFn}
import teststate.typeclass._

object CollectionAssertions {

  private def formatSet(s: IterableOnce[_]): String =
    s.iterator.mkString(", ")

  protected final def tallyElements[A](neg: IterableOnce[A], pos: IterableOnce[A]): mutable.HashMap[A, Int] = {
    val m = mutable.HashMap.empty[A, Int]
    def go(as: IterableOnce[A], n: Int): Unit =
      for (a <- as.iterator)
        m.update(a, m.get(a).fold(n)(_ + n))
    go(neg, -1)
    go(pos, 1)
    m
  }

  case class EA[@specialized(Int) A](expect: A, actual: A)

  // ===================================================================================================================

  // Depends on A having universal equality and appropriate hashcodes
  sealed abstract class Distinct {
    def name(subject: => String): Name
    def apply[A](as: IterableOnce[A])(implicit s: Display[A]): Option[Distinct.Failure[A]]

    protected final def prep[A](as: IterableOnce[A]) = {
      val m = mutable.HashMap.empty[A, Int]
      for (a <- as.iterator) {
        val v = m.getOrElse(a, 0) + 1
        m.update(a, v)
      }
      m
    }

    protected final def pass(m: mutable.HashMap[_, Int]): Boolean =
      m.valuesIterator.forall(_ == 1)
  }

  object Distinct {
    def apply(positive: Boolean): Distinct =
      if (positive) Pos else Neg

    object Pos extends Distinct {
      override def name(subject: => String): Name =
        subject + " should be distinct."

      override def apply[A](as: IterableOnce[A])(implicit s: Display[A]) = {
        val m = prep(as)
        if (pass(m))
          None
        else
          Some(Wasnt(
            m.iterator
              .filter(_._2 > 1)
              .map(x => (x, s(x._1)))
              .toList
              .sortBy(_._2)
              .map(_._1)
          ))
      }
    }

    object Neg extends Distinct {
      override def name(subject: => String): Name =
        subject + " should contain duplicates."

      override def apply[A](as: IterableOnce[A])(implicit s: Display[A]) =
        if (pass(prep(as)))
          Some(Was)
        else
          None
    }

    sealed trait Failure[+A] extends HasErrorString with Product with Serializable

    final case class Wasnt[+A](dups: List[(A, Int)])(implicit s: Display[A]) extends Failure[A] {
      def dupsToString = formatSet(dups.iterator.map { case (a, i) => s"${s(a)} → $i" })
      override def errorString = s"Dups: $dupsToString"
    }

    case object Was extends Failure[Nothing] {
      override def errorString = "No duplicates found."
    }
  }

  // ===================================================================================================================

  sealed abstract class Contains {
    def name(subject: => String, queryName: => String): Name
    def apply[A, B](source: IterableOnce[A], query: B)(implicit ev: A <:< B, eb: Equal[B], sb: Display[B]): Option[Contains.Failure[B]]
    protected final def found[A, B](source: IterableOnce[A], query: B)(implicit ev: A <:< B, eb: Equal[B]) =
      source.iterator.exists(a => eb.equal(query, a))
  }

  object Contains {
    def name(expect: Boolean, subject: => String, queryName: => String): Name =
      Name.lazily(apply(expect).name(subject, queryName))

    def nameFn[I](expect: I => Boolean, subject: => String, queryName: => String): NameFn[I] =
      NameFn {
        case None    => s"$subject: possible existence of $queryName."
        case Some(i) => name(expect(i), subject, queryName)
      }

    def apply(positive: Boolean): Contains =
      if (positive) Pos else Neg

    object Pos extends Contains {
      override def name(subject: => String, queryName: => String): Name =
        s"$subject should contain $queryName."

      override def apply[A, B](source: IterableOnce[A], query: B)(implicit ev: A <:< B, eb: Equal[B], sb: Display[B]): Option[Missing[B]] =
        if (found(source, query))
          None
        else
          Some(Missing(query))
    }

    object Neg extends Contains {
      override def name(subject: => String, queryName: => String): Name =
        s"$subject shouldn't contain $queryName."

      override def apply[A, B](source: IterableOnce[A], query: B)(implicit ev: A <:< B, eb: Equal[B], sb: Display[B]): Option[Present.type] =
        if (found(source, query))
          Some(Present)
        else
          None
    }

    sealed trait Failure[+A] extends HasErrorString with Product with Serializable

    case class Missing[A](query: A)(implicit s: Display[A]) extends Failure[A] {
      override def errorString = s"Not found: ${s(query)}"
    }

    case object Present extends Failure[Nothing] {
      override def errorString = ""
    }
  }

  // ===================================================================================================================

  sealed abstract class ContainsAll {
    def name(subject: => String, queryNames: => String): Name
    def apply[A: UnivEq, B](source: IterableOnce[A], query: Set[B])(implicit ev: B <:< A, sb: Display[B]): Option[ContainsAll.Failure[B]]
    protected final def missing[A: UnivEq, B](source: IterableOnce[A], query: Set[B])(implicit ev: B <:< A) = {
      val as = UnivEq.toSet(source)
      query.iterator.filterNot(as contains _)
    }
  }

  object ContainsAll {
    def apply(positive: Boolean): ContainsAll =
      if (positive) Pos else Neg

    /** ∀b. A ∋ b */
    object Pos extends ContainsAll {
      override def name(subject: => String, queryNames: => String): Name =
        s"$subject should contain all $queryNames."

      override def apply[A: UnivEq, B](source: IterableOnce[A], query: Set[B])(implicit ev: B <:< A, sb: Display[B]): Option[Missing[B]] = {
        val m = missing(source, query)
        if (m.isEmpty)
          None
        else
          Some(Missing(m.toSet, query))
      }
    }

    /** ∃b. A ∌ b */
    object Neg extends ContainsAll {
      override def name(subject: => String, queryNames: => String): Name =
        s"$subject shouldn't contain all $queryNames."

      override def apply[A: UnivEq, B](source: IterableOnce[A], query: Set[B])(implicit ev: B <:< A, sb: Display[B]): Option[AllPresent.type] =
        if (missing(source, query).isEmpty)
          Some(AllPresent)
        else
          None
    }

    sealed trait Failure[+A] extends HasErrorString with Product with Serializable

    case class Missing[A](missing: Set[A], query: Set[A])(implicit s: Display[A]) extends Failure[A] {
      def missingToString = formatSet(missing.iterator.map(s(_)))
      override def errorString = s"Missing: $missingToString"
    }

    case object AllPresent extends Failure[Nothing] {
      override def errorString = "All members found."
    }
  }

  // ===================================================================================================================

  sealed abstract class ContainsAny {
    def name(subject: => String, queryNames: => String): Name
    def apply[A, B](source: IterableOnce[A], query: Set[B])(implicit ev: A <:< B, sb: Display[B]): Option[ContainsAny.Failure[B]]
  }

  /** ∃b. A ∋ b */
  object ContainsSome extends ContainsAny {
    def name(subject: => String, queryNames: => String): Name =
      s"$subject should contain some $queryNames."

    def apply[A, B](source: IterableOnce[A], query: Set[B])(implicit ev: A <:< B, sb: Display[B]): Option[ContainsAny.FoundNone.type] =
      if (source.iterator.exists(query contains _))
        None
      else
        Some(ContainsAny.FoundNone)
  }

  /** ∀b. A ∌ b */
  object ContainsNone extends ContainsAny {
    def name(subject: => String, queryNames: => String): Name =
      s"$subject shouldn't contain any $queryNames."

    def apply[A, B](source: IterableOnce[A], blacklist: Set[B])(implicit ev: A <:< B, sb: Display[B]): Option[ContainsAny.FoundSome[B]] = {
      var bad = Vector.empty[B]
      for (a <- source.iterator)
        if (blacklist contains a)
          bad :+= a.asInstanceOf[B]
      if (bad.isEmpty)
        None
      else
        Some(ContainsAny.FoundSome(bad))
    }
  }

  object ContainsAny {
    def apply(positive: Boolean): ContainsAny =
      if (positive) ContainsSome else ContainsNone

    sealed trait Failure[+A] extends HasErrorString with Product with Serializable

    case object FoundNone extends Failure[Nothing] {
      override def errorString = "None found."
    }

    final case class FoundSome[+A](bad: Vector[A])(implicit s: Display[A]) extends Failure[A] {
      def badToString = formatSet(bad.iterator.map(s(_)))
      override def errorString = s"Found: $badToString"
    }
  }

  // ===================================================================================================================

  sealed abstract class ContainsOnly {
    def name(subject: => String, whitelistNames: => String): Name
    def apply[A: UnivEq, B](source: IterableOnce[A], whitelist: Set[B])(implicit ev: A <:< B, sa: Display[A]): Option[ContainsOnly.Failure[A]]
    protected final def missing[A: UnivEq, B](source: IterableOnce[A], whitelist: Set[B])(implicit ev: B <:< A) = {
      val as = UnivEq.toSet(source)
      whitelist.iterator.filterNot(as contains _)
    }
  }

  object ContainsOnly {
    def apply(positive: Boolean): ContainsOnly =
      if (positive) Pos else Neg

    /** ∀a. a ∈ B */
    object Pos extends ContainsOnly {
      override def name(subject: => String, whitelistNames: => String): Name =
        s"$subject should only contain $whitelistNames."

      override def apply[A: UnivEq, B](source: IterableOnce[A], whitelist: Set[B])(implicit ev: A <:< B, sa: Display[A]) = {
        var bad = Vector.empty[A]
        for (a <- source.iterator)
          if (!whitelist.contains(a))
            bad :+= a
        if (bad.isEmpty)
          None
        else
          Some(FoundIllegal(bad))
      }
    }

    /** ∃a. a ∉ B */
    object Neg extends ContainsOnly {
      override def name(subject: => String, whitelistNames: => String): Name =
        s"$subject should contain other than $whitelistNames."

      override def apply[A: UnivEq, B](source: IterableOnce[A], whitelist: Set[B])(implicit ev: A <:< B, sa: Display[A]) =
        if (source.iterator.exists(!whitelist.contains(_)))
          None
        else
          Some(NothingOffWhitelist)
    }

    sealed trait Failure[+A] extends HasErrorString with Product with Serializable

    case class FoundIllegal[+A](illegal: Vector[A])(implicit s: Display[A]) extends Failure[A] {
      def illegalToString = formatSet(illegal.iterator.map(s(_)))
      override def errorString = s"Found: $illegalToString"
    }

    case object NothingOffWhitelist extends Failure[Nothing] {
      override def errorString = "None found."
    }
  }

  // ===================================================================================================================

  // TODO: Need sub/super-set matchers. This is just a specific case.
  // ∀/∃ ⊆ ⊇ ⊂ ⊃ ⊄ ⊅ ⊈ ⊉

  object ExistenceOfAll {
    def name(expect: Boolean, subject: => String, allNames: => String): Name =
      Name.lazily(
        if (expect)
          ContainsAll.Pos.name(subject, allNames)
        else
          ContainsNone.name(subject, allNames))

    def nameFn[I](expect: I => Boolean, subject: => String, allNames: => String): NameFn[I] =
      NameFn {
        case None    => s"$subject: possible existence of $allNames."
        case Some(i) => name(expect(i), subject, allNames)
      }

    def apply[A: Display: UnivEq](expect: Boolean, source: IterableOnce[A], all: Set[A]): Option[Either[ContainsAny.FoundSome[A], ContainsAll.Missing[A]]] =
      if (expect)
        ContainsAll.Pos(source, all).map(Right(_))
      else
        ContainsNone(source, all).map(Left(_))
  }

  // ===================================================================================================================

  sealed abstract class EqualIgnoringOrder {
//    def name(subject: => String, expectName: => String): Name
    def apply[A](source: IterableOnce[A], expect: IterableOnce[A])(implicit s: Display[A]): Option[EqualIgnoringOrder.Failure[A]]

    protected final def prep[A](source: IterableOnce[A], expect: IterableOnce[A]) =
      tallyElements(expect, source)

    protected final def pass(m: mutable.HashMap[_, Int]): Boolean =
      m.valuesIterator.forall(_ == 0)
  }

  object EqualIgnoringOrder {
    def apply(positive: Boolean): EqualIgnoringOrder =
      if (positive) Pos else Neg

    object Pos extends EqualIgnoringOrder {
//      override def name(subject: => String, expectName: => String): Name =
//        s"$subject should equal $expectName ignoring order."

      override def apply[A](source: IterableOnce[A], expect: IterableOnce[A])(implicit s: Display[A]) = {
        val m = prep(source, expect)
        if (pass(m))
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
            Mismatch(missing.result(), excess.result())
          }
      }
    }

    object Neg extends EqualIgnoringOrder {
//      override def name(subject: => String, expectName: => String): Name =
//        s"$subject shouldn't equal $expectName ignoring order."

      override def apply[A](source: IterableOnce[A], expect: IterableOnce[A])(implicit s: Display[A]) =
        if (pass(prep(source, expect)))
          Some(Matched)
        else
          None
    }

    sealed trait Failure[+A] extends HasErrorString with Product with Serializable

    case class Mismatch[+A](missing: Vector[A], excess: Vector[A])(implicit s: Display[A]) extends Failure[A] {
      private def fmt[AA >: A](name: String, as: Vector[AA])(implicit s: Display[AA]): Option[String] =
        if (as.isEmpty) None else Some(name + ": " +formatSet(as.iterator.map(s(_))) + ".")
      override def errorString =
        (fmt("Missing", missing).toList ::: fmt("Excess", excess).toList).mkString(" ")
    }

    case object Matched extends Failure[Nothing] {
      override def errorString = "Set members match."
    }
  }

  // ===================================================================================================================

  sealed abstract class EqualIncludingOrder {
    def apply[A](source: IterableOnce[A], expect: IterableOnce[A])(implicit eq: Equal[A], s: Display[A]): Option[EqualIncludingOrder.Failure[A]]
  }

  // TODO Could do much better. Should diff
  object EqualIncludingOrder {
    def apply(positive: Boolean): EqualIncludingOrder =
      if (positive) Pos else Neg

    object Pos extends EqualIncludingOrder {
      override def apply[A](source: IterableOnce[A], expect: IterableOnce[A])(implicit eq: Equal[A], s: Display[A]) = {
        val a = source.iterator.toVector
        val e = expect.iterator.toSeq
        if (a.corresponds(e)(eq.equal))
          None
        else
          Some(Mismatch(a, e.toVector))
      }
    }

    object Neg extends EqualIncludingOrder {
      override def apply[A](source: IterableOnce[A], expect: IterableOnce[A])(implicit eq: Equal[A], s: Display[A]) =
        if (source.iterator.toSeq.corresponds(expect.iterator.toSeq)(eq.equal))
          Some(Matched)
        else
          None
    }

    sealed trait Failure[+A] extends HasErrorString with Product with Serializable

    case class Mismatch[+A](actual: Vector[A], expect: Vector[A])(implicit s: Display[A]) extends Failure[A] {
      private def fmt[AA >: A](name: String, as: Vector[AA])(implicit s: Display[AA]): String =
        name + ": " +formatSet(as.iterator.map(s(_)))
      override def errorString =
        fmt("Actual", actual) + "\n" + fmt("Expect", expect)
    }

    case object Matched extends Failure[Nothing] {
      override def errorString = "Set members match."
    }
  }

  // ===================================================================================================================

  sealed abstract class ElemChanges {
    import ElemChanges._

    def apply[A](args: Args[A])(implicit s: Display[A]): Option[Failure[A]]

    protected final def prep[A](args: Args[A]): Option[Map[A, EA[Int]]] = {
      val actual = tallyElements(args.before, args.after)
      val expect = tallyElements(args.expectDel, args.expectAdd)

      var errors = Map.empty[A, EA[Int]]

      for ((k, a) <- actual) {
        val e = expect.getOrElse(k, 0)
        if (a != e)
          errors = errors.updated(k, EA(expect = e, actual = a))
      }

      for ((k, e) <- expect)
        if (e != 0 && !actual.contains(k))
          errors = errors.updated(k, EA(expect = e, actual = 0))

      if (errors.isEmpty)
        None
      else
        Some(errors)
    }
  }

  object ElemChanges {
    case class Args[A](before   : IterableOnce[A],
                       after    : IterableOnce[A],
                       expectDel: IterableOnce[A],
                       expectAdd: IterableOnce[A])

    def apply(positive: Boolean): ElemChanges =
      if (positive) Pos else Neg

    object Pos extends ElemChanges {
      override def apply[A](args: Args[A])(implicit s: Display[A]) =
        prep(args).map(Mismatch(_))
    }

    object Neg extends ElemChanges {
      override def apply[A](args: Args[A])(implicit s: Display[A]) =
        prep(args) match {
          case Some(errors) => None
          case None         => Some(Matched())
        }
    }

    sealed trait Failure[A] extends HasErrorString with Product with Serializable

    case class Mismatch[A](errors: Map[A, EA[Int]])(implicit s: Display[A]) extends Failure[A] {
      override def errorString =
        errors.iterator
          .map { case (i, EA(e, a)) => s"${s(i)} moved by $a, expected $e." }
          .mkString(" ")
    }

    case class Matched[A]() extends Failure[A] {
      override def errorString = "Expected changes occurred."
    }
  }

  // ===================================================================================================================

  sealed abstract class Forall {
    def name(collection: => String, criteria: => String): Name
    def apply[C[x] <: IterableOnce[x], A](as: C[A])(f: A => Boolean)(implicit d: Display[A]): Option[Forall.Failure[A]]
  }

  object Forall {
    def apply(positive: Boolean): Forall =
      if (positive) Pos else Neg

    object Pos extends Forall {
      override def name(coll: => String, criteria: => String): Name =
        s"All $coll should $criteria."

      override def apply[C[x] <: IterableOnce[x], A](as: C[A])(f: A => Boolean)(implicit d: Display[A]): Option[Failed[A]] = {
        var ok = 0
        var ko = Vector.empty[A]
        for (a <- as.iterator)
          if (f(a))
            ok += 1
          else
            ko :+= a

        ko.lastOption.map(last => Failed(ko.init, last, ok))
      }
    }

    object Neg extends Forall {
      override def name(coll: => String, criteria: => String): Name =
        s"Not all $coll should $criteria."

      override def apply[C[x] <: IterableOnce[x], A](as: C[A])(f: A => Boolean)(implicit d: Display[A]) =
        Exists.Pos(as)(!f(_)).map(e => AllPass(e.values))
    }

    sealed trait Failure[+A] extends HasErrorString with Product with Serializable

    final case class Failed[+A](valuesI: Vector[A], valuesL: A, ok: Int)(implicit d: Display[A]) extends Failure[A] {
      val values = valuesI :+ valuesL
      val ko = values.length
      val total = ok + ko
      def failedElementsStr = formatSet(values.iterator.map(d.apply))
      override def errorString = s"$ko of $total elements failed: $failedElementsStr"
    }

    final case class AllPass[+A](values: Vector[A])(implicit d: Display[A]) extends Failure[A] {
      override def errorString = s"All ${values.length} elements satisfied criteria."
    }
  }

  sealed abstract class Exists {
    def name(collection: => String, criteria: => String): Name
    def apply[C[x] <: IterableOnce[x], A](as: C[A])(f: A => Boolean)(implicit d: Display[A]): Option[Exists.Failure[A]]
  }

  object Exists {
    def apply(positive: Boolean): Exists =
      if (positive) Pos else Neg

    object Pos extends Exists {
      override def name(coll: => String, criteria: => String): Name =
        s"Of all $coll, at least one should $criteria."

      override def apply[C[x] <: IterableOnce[x], A](as: C[A])(f: A => Boolean)(implicit d: Display[A]): Option[DoesntExist[A]] = {
        val i = as.iterator
        val b = Vector.newBuilder[A]

        @tailrec def loop(): Option[DoesntExist[A]] =
          if (i.hasNext) {
            val a = i.next()
            if (f(a))
              None
            else {
              b += a
              loop()
            }
          } else
            Some(DoesntExist(b.result()))

        loop()
      }
    }

    object Neg extends Exists {
      override def name(coll: => String, criteria: => String): Name =
        s"Of all $coll, none should $criteria."

      override def apply[C[x] <: IterableOnce[x], A](as: C[A])(f: A => Boolean)(implicit d: Display[A]): Option[Passed[A]] =
        Forall.Pos(as)(!f(_)).map(e => Passed(e.valuesI, e.valuesL, e.ok))
    }

    sealed trait Failure[+A] extends HasErrorString with Product with Serializable

    final case class DoesntExist[+A](values: Vector[A])(implicit d: Display[A]) extends Failure[A] {
      override def errorString = s"None of the ${values.length} elements satisfied criteria."
    }

    final case class Passed[+A](valuesI: Vector[A], valuesL: A, ok: Int)(implicit d: Display[A]) extends Failure[A] {
      val values = valuesI :+ valuesL
      val ko = values.length
      val total = ok + ko
      def passedElementsStr = formatSet(values.iterator.map(d.apply))
      override def errorString = s"$ko of $total elements satisfied criteria: $passedElementsStr"
    }
  }
}
