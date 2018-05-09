package teststate.dsl

import scala.annotation.tailrec
import utest._
import teststate.Exports._
import teststate.TestUtil._
import CollectionAssertions._

object CollectionAssertionsTest extends TestSuite {

  def set(s: String): Set[Char] =
    s.toCharArray.toSet

  val z    = set("")
  val a    = set("a")
  val d    = set("d")
  val ab   = set("ab")
  val cd   = set("cd")
  val abc  = set("abc")
  val bcd  = set("bcd")
  val abcd = set("abcd")

  val data = {
    val n0 = 0 to 2

    for {
      a1 <- n0
      b1 <- n0
      c1 <- n0
      a2 <- n0
      b2 <- n0
    } yield {
      val sb = new StringBuilder
      @tailrec def add(c: Char, n: Int): Unit =
        if (n != 0) {
        sb append c
          add(c, n - 1)
      }
      add('a', a1)
      add('b', b1)
      add('c', c1)
      add('a', a2)
      add('b', b2)
      sb.result()
    }
  }
  val dataSets = data.map(_.toSet)

  val dataB = {
    val n0 = 0 to 1

    for {
      a1 <- n0
      b1 <- n0
      c1 <- n0
    } yield {
      val sb = new StringBuilder
      @tailrec def add(c: Char, n: Int): Unit =
        if (n != 0) {
        sb append c
          add(c, n - 1)
      }
      add('a', a1)
      add('b', b1)
      add('c', c1)
      sb.result()
    }
  }

  val dataSetsB = dataB.map(_.toSet)

  type D = String
  type DS = Set[Char]

  val charPred = (_: Char) <= 'b'

  override def tests = TestSuite {
    'logic {

      def test1(name: String, f: (Boolean, D) => Option[Any], expectPass: D => Boolean) =
        for (d <- data) {
          val e = expectPass(d)
          def n = s"$name: [$d]"
          assertDefined(n, f(true, d), !e)
          assertDefined("¬" + n, f(false, d), e)
        }

      def test2(name: String, f: (Boolean, D, DS) => Option[Any], expectPass: (D, DS) => Boolean) =
        for {d <- data; s <- dataSets} {
          val e = expectPass(d, s)
          def n = s"$name: [$d] $s"
          assertDefined(n, f(true, d, s), !e)
          assertDefined("¬" + n, f(false, d, s), e)
        }

      def test2d(name: String, f: (Boolean, D, D) => Option[Any], expectPass: (D, D) => Boolean) =
        for {d <- data; s <- data} {
          val e = expectPass(d, s)
          def n = s"$name: [$d] [$s]"
          assertDefined(n, f(true, d, s), !e)
          assertDefined("¬" + n, f(false, d, s), e)
        }

      def test4(name: String, f: (Boolean, DS, DS, DS, DS) => Option[Any], expectPass: (DS, DS, DS, DS) => Boolean) =
        for {a <- dataSetsB; b <- dataSetsB; c <- dataSetsB; d <- dataSetsB} {
          val e = expectPass(a, b, c, d)
          def n = s"$name: [$a] [$b] [$c] [$d]"
          assertDefined(n, f(true, a, b, c, d), !e)
          assertDefined("¬" + n, f(false, a, b, c, d), e)
        }

      'containsAll  - test2("containsAll" , ContainsAll (_)(_, _), (d, s) => s.forall(d contains _))
      'containsAny  - test2("containsAny" , ContainsAny (_)(_, _), (d, s) => s.exists(d contains _))
      'containsAny2 - test2("containsAny2", ContainsAny (_)(_, _), (d, s) => d.exists(s.contains))
      'containsOnly - test2("containsOnly", ContainsOnly(_)(_, _), (d, s) => d.forall(s.contains))

      'distinct - test1("distinct", Distinct(_)(_), d => d.sorted.distinct.length == d.length)

      'equalIgnoringOrder - test2d("equalIgnoringOrder", EqualIgnoringOrder(_)(_, _), (a, b) => a.sorted == b.sorted)

      'equalIncludingOrder - test2d("equalIncludingOrder", EqualIncludingOrder(_)(_, _), (a, b) => a == b)

      'elemChanges - test4("elemChanges", (p, a, b, c, d) => ElemChanges(p)(ElemChanges.Args(a, b, c, d)),
        (b, a, ed, ea) => {
          val c = ea & ed
          (ea -- c) == (a -- b) &&
          (ed -- c) == (b -- a)
        })

      'forall - test1("forall", (b, d) => Forall(b)(d.toList)(charPred), _ forall charPred)

      'exists - test1("exists", (b, d) => Exists(b)(d.toList)(charPred), _ exists charPred)
    }


    'text {
      def testNoName[F](f: F)(test: F => Option[HasErrorString], expectedError: String): Unit =
        assertEq(test(f).map(_.errorString), Some(expectedError))

      def test[F](f: F)(name: F => Name, expectedName: String)(test: F => Option[HasErrorString], expectedError: String): Unit = {
        assertEq(name(f).value, expectedName)
        testNoName(f)(test, expectedError)
      }

      'containsAllP - test(ContainsAll(true))(
        _.name("A", "B"), "A should contain all B.")(
        _ ("abc", "cde".toSet), "Missing: 'd', 'e'")

      'containsAllF - test(ContainsAll(false))(
        _.name("A", "B"), "A shouldn't contain all B.")(
        _ ("abcde", "cd".toSet), "All members found.")

      'containsAnyP - test(ContainsAny(true))(
        _.name("A", "B"), "A should contain some B.")(
        _ ("abc", "xy".toSet), "None found.")

      'containsAnyF - test(ContainsAny(false))(
        _.name("A", "B"), "A shouldn't contain any B.")(
        _ ("abcde", "cdx".toSet), "Found: 'c', 'd'")

      'containsOnlyP - test(ContainsOnly(true))(
        _.name("A", "B"), "A should only contain B.")(
        _ ("abcde", "bcd".toSet), "Found: 'a', 'e'")

      'containsOnlyF - test(ContainsOnly(false))(
        _.name("A", "B"), "A should contain other than B.")(
        _ ("bcdbcd", "bcd".toSet), "None found.")

      'distinctP - test(Distinct(true))(
        _.name("A"), "A should be distinct.")(
        _ ("beabcdbfe"), "Dups: 'b' → 3, 'e' → 2")

      'distinctF - test(Distinct(false))(
        _.name("A"), "A should contain duplicates.")(
        _ ("abcde"), "No duplicates found.")

      'equalIgnoringOrderP - testNoName(EqualIgnoringOrder(true))(
        _ ("abcdefa", "cdfex"), "Missing: 'x'. Excess: 'b', 'a', 'a'.")

      'equalIgnoringOrderF - testNoName(EqualIgnoringOrder(false))(
        _ ("qwe", "qwe"), "Set members match.")

      'equalIncludingOrderP - testNoName(EqualIncludingOrder(true))(
        _ ("abc", "acb"), "Actual: 'a', 'b', 'c'\nExpect: 'a', 'c', 'b'")

      'equalIncludingOrderF - testNoName(EqualIncludingOrder(false))(
        _ ("qwe", "qwe"), "Set members match.")

      'elemChanges {
        'neg - testNoName(ElemChanges(false))(
          _ (ElemChanges.Args[Int](Nil, Nil, Nil, Nil)), "Expected changes occurred.")

        'ea - testNoName(ElemChanges(true))(
          _ (ElemChanges.Args(Nil, Nil, Nil, 'a' :: Nil)), "'a' moved by 0, expected 1.")

        'ed - testNoName(ElemChanges(true))(
          _ (ElemChanges.Args(Nil, Nil, 'd' :: Nil, Nil)), "'d' moved by 0, expected -1.")

        'aa - testNoName(ElemChanges(true))(
          _ (ElemChanges.Args(Nil, 'a' :: Nil, Nil, Nil)), "'a' moved by 1, expected 0.")

        'ad - testNoName(ElemChanges(true))(
          _ (ElemChanges.Args('d' :: Nil, Nil, Nil, Nil)), "'d' moved by -1, expected 0.")
      }

      'contains {
        val nameFn = Contains.nameFn(identity[Boolean], "Bag", "Malazan")
        'nameA - assertEq(nameFn(None)       .value, "Bag: possible existence of Malazan.")
        'nameT - assertEq(nameFn(Some(true)) .value, "Bag should contain Malazan.")
        'nameF - assertEq(nameFn(Some(false)).value, "Bag shouldn't contain Malazan.")
      }

      'existenceOfAll {
        val nameFn = ExistenceOfAll.nameFn(identity[Boolean], "Bag", "books")
        'nameA - assertEq(nameFn(None)       .value, "Bag: possible existence of books.")
        'nameT - assertEq(nameFn(Some(true)) .value, "Bag should contain all books.")
        'nameF - assertEq(nameFn(Some(false)).value, "Bag shouldn't contain any books.")
      }

      /*
      'forallP - test(Forall(true))(
        _.name("Ints", "large"), "∀ Ints. large")(
        _ (List(1,9,2))(_ > 5), "2 of 3 elements failed: 1, 2.")

      'forallF - test(Forall(false))(
        _.name("Ints", "small"), "¬ ∀ Ints. small")(
        _ (List(1,2,3))(_ < 5), "3 elements found; all passed.")

      'existsP - test(Exists(true))(
        _.name("Ints", "large"), "∃ Ints. large")(
        _ (List(1,2,3))(_ > 5), "3 elements found; none passed.")

      'existsF - test(Exists(false))(
        _.name("Ints", "large"), "¬ ∃ Ints. large")(
        _ (List(1,9,3))(_ > 5), "1 of 3 elements exist: 9.")
       */

      'forallP - test(Forall(true))(
        _.name("ints", "be large"), "All ints should be large.")(
        _ (List(1,9,2))(_ > 5), "2 of 3 elements failed: 1, 2")

      'forallF - test(Forall(false))(
        _.name("ints", "be large"), "Not all ints should be large.")(
        _ (List(7,8,9))(_ > 5), "All 3 elements satisfied criteria.")

      'existsP - test(Exists(true))(
        _.name("ints", "be large"), "Of all ints, at least one should be large.")(
        _ (List(1,2,3))(_ > 5), "None of the 3 elements satisfied criteria.")

      'existsF - test(Exists(false))(
        _.name("ints", "be large"), "Of all ints, none should be large.")(
        _ (List(1,9,3))(_ > 5), "1 of 3 elements satisfied criteria: 9")

    }
  }
}
