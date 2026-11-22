package teststate.domzipper

import cats.instances.either._
import cats.instances.int._
import cats.instances.option._
import cats.instances.string._
import cats.instances.vector._
import japgolly.microlibs.testutil.TestUtil._
import utest._

object DomCollectionTraverseTest extends TestSuite {

  override def tests = Tests {

    "empty" - {
      val res = ErrorHandler.ReturnEither.C0N.traverse(Vector.empty[Int])(i => Right(i + 1))
      assertEq(res, Right(Vector.empty[Int]))
    }

    "preservesOrder" - {
      val res = ErrorHandler.ReturnEither.C0N.traverse(Vector(1, 2, 3))(i => Right(i * 10))
      assertEq(res, Right(Vector(10, 20, 30)))
    }

    "shortCircuitsOnFailure" - {
      var seen = Vector.empty[Int]
      val res = ErrorHandler.ReturnEither.C0N.traverse(Vector(1, 2, 3, 4)) { i =>
        seen :+= i
        if (i == 2) Left("boom") else Right(i)
      }
      assertEq(res, Left("boom"))
      assertEq(seen, Vector(1, 2))
    }

    "stackSafe" - {
      val n = 20000
      val input = Vector.tabulate(n)(identity)
      val expected = input.map(_ + 1)

      "either" - {
        assertEq(ErrorHandler.ReturnEither.C0N.traverse(input)(i => Right(i + 1)), Right(expected))
      }

      "option" - {
        assertEq(ErrorHandler.ReturnOption.C1N.traverse(input)(i => Some(i + 1)), Some(expected))
      }

      "throw" - {
        assertEq(ErrorHandler.Throw.C0N.traverse(input)(_ + 1), expected)
      }
    }

  }
}
