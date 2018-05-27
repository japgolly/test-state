package teststate

import Exports._

object TestUtil extends TestUtil

trait TestUtil
  extends japgolly.microlibs.testutil.TestUtil {

  implicit def testStateEqualityToScalaz[A](implicit e: Equal[A]): scalaz.Equal[A] =
    scalaz.Equal.equal(e.equal)

  val inspectionBaseSettings =
    Report.Format.Default
      .uncoloured
      .copy(stats = Report.Format.Default.StatsFormat.withoutTime)

  val inspectionFormatOnlyFailedChildren =
    inspectionBaseSettings.onlyShowFailedChildren.apply

  val inspectionFormat =
    inspectionBaseSettings.alwaysShowChildren.apply

  val inspectionAS =
    Report.AssertionSettings.uniform(inspectionFormat)

  def assertDefined[A](o: Option[A], expectDefined: Boolean): Unit =
    assertDefined(null, o, expectDefined)

  def assertDefined[A](name: => String, o: Option[A], expectDefined: Boolean): Unit =
    if (expectDefined)
      assertEq(name, o.isDefined, true)
    else
      assertEq(name, o, None)(scalaz.Equal.equalA)

  val trim = (_: String).trim
  val stringIdFn = (s: String) => s

  def assertRun[E](r           : Report[E],
                   expect      : String,
                   showChildren: Boolean          = true,
                   normalise   : String => String = stringIdFn)
                  (implicit s  : DisplayError[E]): Unit = {
    val n = normalise compose trim
    val f = if (showChildren) inspectionFormat else inspectionFormatOnlyFailedChildren
    val actual = r.format(f)(s)
    assertMultiline(actual = n(actual), n(expect))
  }
}

case class ActualExpect[A](actual: A, expect: A) {
  def map[B](f: A => B) = ActualExpect[B](f(actual), f(expect))

  def forall(f: A => Boolean) = f(actual) && f(expect)
  def exists(f: A => Boolean) = f(actual) || f(expect)
}
