package teststate.domzipper

import cats.instances.either._
import cats.instances.int._
import cats.instances.option._
import cats.instances.string._
import japgolly.microlibs.testutil.TestUtil._
import utest._

object ErrorHandlerTest extends TestSuite {

  override def tests = Tests {

    "attemptSuccess" - {
      assertEq(ErrorHandler.ReturnEither.attempt(42), Right(42))
    }

    "attemptWithMessage" - {
      assertEq(ErrorHandler.ReturnEither.attempt[Int](throw new RuntimeException("boom")), Left("boom"))
    }

    "attemptWithNullMessage" - {
      val ex = new NullPointerException()
      assertEq(ErrorHandler.ReturnEither.attempt[Int](throw ex), Left(ex.toString))
    }

    "attemptReturnOption" - {
      assertEq(ErrorHandler.ReturnOption.attempt[Int](throw new NullPointerException()), None)
    }

    "attemptThrowRethrows" - {
      val ex = new NullPointerException()
      val thrown = intercept[NullPointerException](ErrorHandler.Throw.attempt[Unit](throw ex))
      assert(thrown eq ex)
    }

  }
}
