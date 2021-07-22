package teststate.selenium

import org.openqa.selenium.WebDriver
import scala.concurrent.duration.Duration
import teststate.data.{Id, Or}
import teststate.dsl.Dsl
import teststate.selenium.TestStateExt._
import teststate.typeclass.ExecutionModel

object TestStateExt {

  final class DslIdExt[R, O, S, E](private val dsl: Dsl[Id, R, O, S, E]) extends AnyVal {

    /** Make sure this comes after any other .withActionMod calls. */
    def withSeleniumTab(tab: R => Tab[WebDriver]): Dsl[Id, R, O, S, E] =
      dsl.withActionMod(_.mod(actionDef => ros =>
        actionDef(ros).map(actionFn =>
          () => tab(ros.ref).use(_ => actionFn()))))
  }

  final class DslExt[F[_], R, O, S, E](private val dsl: Dsl[F, R, O, S, E]) extends AnyVal {

    /** Make sure this comes after any other .withActionMod calls. */
    def withSeleniumTab(tab: R => Tab[WebDriver],
                        lockWait: Duration,
                        lockRetry: Duration)
                       (implicit EM: ExecutionModel[F]): Dsl[F, R, O, S, E] = {

      // Scala needs this since partial unification
      type Omfg = E Or (O => E Or S)

      dsl.withActionMod(_.mod(actionDef => ros =>
        actionDef(ros).map(actionFn =>
          () => tab(ros.ref).useM[F, Omfg](_ => actionFn(), lockWait, lockRetry))))
    }
  }

}

trait TestStateExtLowPri {
  implicit def testStateSeleniumDslExt[F[_], R, O, S, E](dsl: Dsl[F, R, O, S, E]): DslExt[F, R, O, S, E] =
    new DslExt(dsl)
}

trait TestStateExt extends TestStateExtLowPri {
  implicit def testStateSeleniumDslIdExt[R, O, S, E](dsl: Dsl[Id, R, O, S, E]): DslIdExt[R, O, S, E] =
    new DslIdExt(dsl)
}
