package teststate.util

import japgolly.microlibs.testutil.TestUtil._
import utest._

object CssUtilTest extends TestSuite {

  override def tests = Tests {
    'disableCssAnimation {
      val actual = CssUtil.disableCssAnimation()
      val expect =
        """
          |* {
          |/*CSS transitions*/
          |-moz-transition-property: none !important;
          |-ms-transition-property: none !important;
          |-o-transition-property: none !important;
          |-webkit-transition-property: none !important;
          |transition-property: none !important;
          |/*CSS transforms*/
          |-moz-transform: none !important;
          |-ms-transform: none !important;
          |-o-transform: none !important;
          |-webkit-transform: none !important;
          |transform: none !important;
          |/*CSS animations*/
          |-moz-animation: none !important;
          |-ms-animation: none !important;
          |-o-animation: none !important;
          |-webkit-animation: none !important;
          |animation: none !important;
          |}
        """.stripMargin.trim
      assertMultiline(actual, expect)
    }
  }
}
