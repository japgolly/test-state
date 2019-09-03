package teststate.util

import japgolly.microlibs.testutil.TestUtil._
import utest._

object CssUtilTest extends TestSuite {

  override def tests = Tests {
    "disableCssAnimation" - {
      val actual = CssUtil.disableCssAnimation()
      val expect =
        """
          |* {
          |/*CSS transitions*/
          |-moz-transition-property:none!important;
          |-ms-transition-property:none!important;
          |-o-transition-property:none!important;
          |-webkit-transition-property:none!important;
          |transition-property:none!important;
          |-moz-transition-delay:0s!important;
          |-ms-transition-delay:0s!important;
          |-o-transition-delay:0s!important;
          |-webkit-transition-delay:0s!important;
          |transition-delay:0s!important;
          |-moz-transition-duration:0s!important;
          |-ms-transition-duration:0s!important;
          |-o-transition-duration:0s!important;
          |-webkit-transition-duration:0s!important;
          |transition-duration:0s!important;
          |/*CSS transforms*/
          |-moz-transform:none!important;
          |-ms-transform:none!important;
          |-o-transform:none!important;
          |-webkit-transform:none!important;
          |transform:none!important;
          |/*CSS animations*/
          |-moz-animation:none!important;
          |-ms-animation:none!important;
          |-o-animation:none!important;
          |-webkit-animation:none!important;
          |animation:none!important;
          |-moz-animation-delay:0s!important;
          |-ms-animation-delay:0s!important;
          |-o-animation-delay:0s!important;
          |-webkit-animation-delay:0s!important;
          |animation-delay:0s!important;
          |-moz-animation-duration:0s!important;
          |-ms-animation-duration:0s!important;
          |-o-animation-duration:0s!important;
          |-webkit-animation-duration:0s!important;
          |animation-duration:0s!important;
          |-moz-animation-iteration-count:0!important;
          |-ms-animation-iteration-count:0!important;
          |-o-animation-iteration-count:0!important;
          |-webkit-animation-iteration-count:0!important;
          |animation-iteration-count:0!important;
          |}
        """.stripMargin.trim
      assertMultiline(actual, expect)
    }
  }
}
