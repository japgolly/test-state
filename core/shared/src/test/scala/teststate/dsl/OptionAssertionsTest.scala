package teststate.dsl

import utest._
import OptionAssertions._
import japgolly.microlibs.testutil.TestUtil._
import teststate.Exports._
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.std.string._

object OptionAssertionsTest extends TestSuite {

  private def testLogic[F](test: Boolean => Option[HasErrorString], expect: Boolean): Unit = {
    assertEq("logic: pos", test(true).isEmpty, expect)
    assertEq("logic: neg", test(false).isDefined, expect)
  }

  private def testName(name: Boolean => Name, expectPos: String, expectNeg: String): Unit = {
    assertEq("name: pos", name(true).value, expectPos)
    assertEq("name: neg", name(false).value, expectNeg)
  }

  private def testError(name: Boolean => Option[HasErrorString], expect: String): Unit = {
    val actual = name(true) orElse name(false)
    assertEq(actual.map(_.errorString), Some(expect))
  }

  private val isOdd = (i: Int) => (i & 1) == 1
  private val ois = List[Option[Int]](None, Some(1), Some(2), Some(3), Some(4))
  private val is = List[Int](1, 2, 3)
  private val fs = List[Int => Boolean](_ == 1, isOdd, _ >= 3)
  private val none = Option.empty[Int]

  override def tests = Tests {

    "contains" - {
      "logic" - {
        for {oi <- ois; i <- is}
          testLogic(Contains(_)(oi, i), oi contains i)
      }
      "name" - {
        testName(Contains(_).name("O", "A"),
          "O should contain A.",
          "O shouldn't contain A.")
      }
      "errs" - {
        "none" - testError(Contains(_)(None, 3), "Option was empty, expected to contain 3.")
        "some" - testError(Contains(_)(Some(1), 2), "Contained 1, expected 2.")
        "neg" - testError(Contains(_)(Some(4), 4), "Shouldn't contain 4.")
      }
    }

    "forall" - {
      "logic" - {
        for {oi <- ois; f <- fs}
          testLogic(Forall(_)(oi)(f), oi forall f)
      }
      "name" - {
        testName(Forall(_).name("Option", "be large"),
          "Option should be empty or be large.",
          "Option should be defined and not be large.")
      }
      "errs" - {
        "none" - testError(Forall(_)(none)(isOdd), "Option was empty.")
        "someP" - testError(Forall(_)(Some(1))(isOdd), "Contained 1.")
        "someF" - testError(Forall(_)(Some(2))(isOdd), "Contained 2.")
      }
    }

    "exists" - {
      "logic" - {
        for {oi <- ois; f <- fs}
          testLogic(Exists(_)(oi)(f), oi exists f)
      }
      "name" - {
        testName(Exists(_).name("Option", "be large"),
          "Option should be defined and be large.",
          "Option should be empty or not be large.")
      }
      "errs" - {
        "none" - testError(Exists(_)(none)(isOdd), "Option was empty.")
        "someP" - testError(Exists(_)(Some(1))(isOdd), "Contained 1.")
        "someF" - testError(Exists(_)(Some(2))(isOdd), "Contained 2.")
      }
    }

  }
}
