package teststate.selenium

import org.openqa.selenium.WebDriver
import teststate.dsl.Dsl
import teststate.typeclass.ExecutionModel

object TestStateExt {

  final class DslExt[F[_], R, O, S, E](private val dsl: Dsl[F, R, O, S, E]) extends AnyVal {

    def withSeleniumTab(tab: R => Tab[WebDriver])(implicit EM: ExecutionModel[F]): Dsl[F, R, O, S, E] =
      dsl.withActionMod(_.mod(actionDef => ros =>
        actionDef(ros).map(actionFn => () =>
          EM.flatten(EM.point(tab(ros.ref).use(_ =>
            actionFn()))))))
  }

}


trait TestStateExt {
  import TestStateExt._

  implicit def testStateSeleniumDslExt[F[_], R, O, S, E](dsl: Dsl[F, R, O, S, E]): DslExt[F, R, O, S, E] =
    new DslExt(dsl)

}
