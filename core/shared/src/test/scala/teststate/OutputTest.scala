package teststate

import utest._
import teststate.Exports.{assertionSettings => _, _}
import teststate.TestUtil._

object OutputTest extends TestSuite {

  val * = Dsl[Unit, Unit, Unit]

  def mockAction(name: String) = *.action(name).act(_ => ())
  def mockPoint (name: String) = *.point(name, _ => None)
  def mockAround(name: String) = *.around(name, _ => ())((_, _) => None)

  val action    = mockAction("Press button.")
  val action2   = mockAction("Pull lever.")
  val actionF   = *.action("Press button!").actTry(_ => Some("BUTTON'S BROKEN"))
  val actionG   = (action >> action2).group("Groupiness.")
  val actionGF1 = (actionF >> action2).group("Groupiness.")
  val actionGF2 = (action >> actionF).group("Groupiness.")
  val actionGS  = actionG.skip
  val actionS   = action.skip
  val action2S  = action2.skip

  val checkPoint   = mockPoint("Check stuff.")
  val checkPoint2  = mockPoint("Check more stuff.")
  val checkPointF  = *.point("Check failure.", _ => Some("Shit broke!"))
  val checkAround  = mockAround("Button count increased.")
  val checkAroundF = *.around("Button count increased.", _ => ())((_, _) => Some("2 != 3"))

  val checkOS = *.focus("Hurp").obsAndState(_ => 1, _ => 1).assert.equal

  val i12 = mockPoint("Invariant 1") & mockPoint("Invariant 2")

  val sub1 =
    Plan(
      mockAction("SubAction!") addCheck mockAround("Sub around-check"),
      mockPoint("Sub-invariant 1"))
    .addInvariants(mockPoint("Sub-invariant 2"))
    .asAction("SubTest!")

  val sub2 =
    sub1
      .addCheck(mockPoint("Sub-pre").before)
      .addCheck(mockPoint("Sub-post").after)
      .addCheck(mockAround("Sub-delta"))

  def test(a: *.Action, i: *.Invariant)(expect: String): Unit = {
    val r = Plan(a, i).stateless.testU.runU
    assertRun(r, expect)
  }

  override def tests = TestSuite {

    'empty {
      'empty - test(emptyAction, emptyInvariants)("- Nothing to do.")

      'before - test(emptyAction addCheck checkPoint.before, emptyInvariants)(
        """
          |✓ 0 actions.
          |  ✓ Pre-conditions
          |    ✓ Check stuff.
          |✓ All pass.
          |Performed 0 actions, 1 check.
        """.stripMargin)

      'after - test(emptyAction addCheck checkPoint.after, emptyInvariants)(
        """
          |✓ 0 actions.
          |  ✓ Post-conditions
          |    ✓ Check stuff.
          |✓ All pass.
          |Performed 0 actions, 1 check.
        """.stripMargin)

      'around - test(emptyAction addCheck checkAround, emptyInvariants)(
        """
          |✓ 0 actions.
          |  ✓ Post-conditions
          |    ✓ Button count increased.
          |✓ All pass.
          |Performed 0 actions, 1 check.
        """.stripMargin)

      'beforeAfter - test(emptyAction addCheck checkPoint.after addCheck checkPoint2.before, emptyInvariants)(
        """
          |✓ 0 actions.
          |  ✓ Pre-conditions
          |    ✓ Check more stuff.
          |  ✓ Post-conditions
          |    ✓ Check stuff.
          |✓ All pass.
          |Performed 0 actions, 2 checks.
        """.stripMargin)
    }

    'invariants {
      def t(i: *.Invariant)(expect: String) = test(emptyAction, i)(expect)
      'pass {
        'simplest - t(checkPoint)(
          """
            |✓ Initial state.
            |  ✓ Check stuff.
            |✓ All pass.
            |Performed 0 actions, 1 check.
          """.stripMargin)

        'multiple - t(checkPoint & checkPoint2)(
          """
            |✓ Initial state.
            |  ✓ Check stuff.
            |  ✓ Check more stuff.
            |✓ All pass.
            |Performed 0 actions, 2 checks.
          """.stripMargin)

        'coproduct - t(*.chooseInvariant("Who knows?!", _ => checkPoint))(
          """
            |✓ Initial state.
            |  ✓ Check stuff.
            |✓ All pass.
            |Performed 0 actions, 1 check.
          """.stripMargin)
      }
      'fail {
        'single - t(checkPointF)(
          """
            |✘ Initial state.
            |  ✘ Check failure. -- Shit broke!
            |Performed 0 actions, 1 check.
          """.stripMargin)

        'first - t(checkPointF & checkPoint2)(
          """
            |✘ Initial state.
            |  ✘ Check failure. -- Shit broke!
            |  ✓ Check more stuff.
            |Performed 0 actions, 2 checks.
          """.stripMargin)

        'second - t(checkPoint & checkPointF)(
          """
            |✘ Initial state.
            |  ✓ Check stuff.
            |  ✘ Check failure. -- Shit broke!
            |Performed 0 actions, 2 checks.
          """.stripMargin)

        'coproduct - t(*.tryChooseInvariant("Who knows?!", _ => Left("Computer says no.")))(
          """
            |✘ Initial state.
            |  ✘ Who knows?! -- Computer says no.
            |Performed 0 actions, 1 check.
          """.stripMargin)
      }
    }

    'action {
      def t(a: *.Action)(expect: String) = test(a, emptyInvariants)(expect)
      'pass {
        'simplest - t(action)(
          """
            |✓ Press button.
            |✓ All pass.
            |Performed 1 action, 0 checks.
          """.stripMargin)

        'before - t(action addCheck checkPoint.before)(
          """
            |✓ Press button.
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✓ Action
            |✓ All pass.
            |Performed 1 action, 1 check.
          """.stripMargin)

        'after - t(action addCheck checkPoint.after)(
          """
            |✓ Press button.
            |  ✓ Action
            |  ✓ Post-conditions
            |    ✓ Check stuff.
            |✓ All pass.
            |Performed 1 action, 1 check.
          """.stripMargin)

        'around - t(action addCheck checkAround)(
          """
            |✓ Press button.
            |  ✓ Action
            |  ✓ Post-conditions
            |    ✓ Button count increased.
            |✓ All pass.
            |Performed 1 action, 1 check.
          """.stripMargin)

        'beforeAfter - t(action addCheck checkPoint.after addCheck checkPoint2.before)(
          """
            |✓ Press button.
            |  ✓ Pre-conditions
            |    ✓ Check more stuff.
            |  ✓ Action
            |  ✓ Post-conditions
            |    ✓ Check stuff.
            |✓ All pass.
            |Performed 1 action, 2 checks.
          """.stripMargin)
      }
      'fail {
        'simplest - t(actionF)(
          """
            |✘ Press button! -- BUTTON'S BROKEN
            |Performed 1 action, 0 checks.
          """.stripMargin)

        'beforeA - t(actionF addCheck checkPoint.before)(
          """
            |✘ Press button!
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✘ Action -- BUTTON'S BROKEN
            |Performed 1 action, 1 check.
          """.stripMargin)

        'beforeC - t(action addCheck checkPointF.before)(
          """
            |✘ Press button.
            |  ✘ Pre-conditions
            |    ✘ Check failure. -- Shit broke!
            |Performed 1 action, 1 check.
          """.stripMargin)

        'afterA - t(actionF addCheck checkPoint.after)(
          """
            |✘ Press button! -- BUTTON'S BROKEN
            |Performed 1 action, 0 checks.
          """.stripMargin)

        'afterC - t(action addCheck checkPointF.after)(
          """
            |✘ Press button.
            |  ✓ Action
            |  ✘ Post-conditions
            |    ✘ Check failure. -- Shit broke!
            |Performed 1 action, 1 check.
          """.stripMargin)

        'around - t(action addCheck checkAroundF)(
          """
            |✘ Press button.
            |  ✓ Action
            |  ✘ Post-conditions
            |    ✘ Button count increased. -- 2 != 3
            |Performed 1 action, 1 check.
          """.stripMargin)

        'beforeAfter1 - t(action addCheck checkPoint.after addCheck checkPointF.before)(
          """
            |✘ Press button.
            |  ✘ Pre-conditions
            |    ✘ Check failure. -- Shit broke!
            |Performed 1 action, 1 check.
          """.stripMargin)

        'beforeAfter2 - t(actionF addCheck checkPoint.after addCheck checkPoint2.before)(
          """
            |✘ Press button!
            |  ✓ Pre-conditions
            |    ✓ Check more stuff.
            |  ✘ Action -- BUTTON'S BROKEN
            |Performed 1 action, 1 check.
          """.stripMargin)

        'beforeAfter3 - t(action addCheck checkPointF.after addCheck checkPoint2.before)(
          """
            |✘ Press button.
            |  ✓ Pre-conditions
            |    ✓ Check more stuff.
            |  ✓ Action
            |  ✘ Post-conditions
            |    ✘ Check failure. -- Shit broke!
            |Performed 1 action, 2 checks.
          """.stripMargin)
      }
    }

    'product {
      val a2 = action >> action2
      'before - test(a2 addCheck checkPoint.before, emptyInvariants)(
        """
          |✓ 2 actions.
          |  ✓ Pre-conditions
          |    ✓ Check stuff.
          |  ✓ Press button.
          |  ✓ Pull lever.
          |✓ All pass.
          |Performed 2 actions, 1 check.
        """.stripMargin)

      'after - test(a2 addCheck checkPoint.after, emptyInvariants)(
        """
          |✓ 2 actions.
          |  ✓ Press button.
          |  ✓ Pull lever.
          |  ✓ Post-conditions
          |    ✓ Check stuff.
          |✓ All pass.
          |Performed 2 actions, 1 check.
        """.stripMargin)

      'around - test(a2 addCheck checkAround, emptyInvariants)(
        """
          |✓ 2 actions.
          |  ✓ Press button.
          |  ✓ Pull lever.
          |  ✓ Post-conditions
          |    ✓ Button count increased.
          |✓ All pass.
          |Performed 2 actions, 1 check.
        """.stripMargin)

      'beforeAfter - test(a2 addCheck checkPoint.after addCheck checkPoint2.before, emptyInvariants)(
        """
          |✓ 2 actions.
          |  ✓ Pre-conditions
          |    ✓ Check more stuff.
          |  ✓ Press button.
          |  ✓ Pull lever.
          |  ✓ Post-conditions
          |    ✓ Check stuff.
          |✓ All pass.
          |Performed 2 actions, 2 checks.
        """.stripMargin)
    }

    'actionG {
      def t(a: *.Action)(expect: String) = test(a, emptyInvariants)(expect)
      'pass {
        'simple - t(actionG)(
          """
            |✓ Groupiness.
            |  ✓ Press button.
            |  ✓ Pull lever.
            |✓ All pass.
            |Performed 2 actions, 0 checks.
          """.stripMargin)

        'before - t(actionG addCheck checkPoint.before)(
          """
            |✓ Groupiness.
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✓ Press button.
            |  ✓ Pull lever.
            |✓ All pass.
            |Performed 2 actions, 1 check.
          """.stripMargin)

        'after - t(actionG addCheck checkPoint.after)(
          """
            |✓ Groupiness.
            |  ✓ Press button.
            |  ✓ Pull lever.
            |  ✓ Post-conditions
            |    ✓ Check stuff.
            |✓ All pass.
            |Performed 2 actions, 1 check.
          """.stripMargin)

        'around - t(actionG addCheck checkAround)(
          """
            |✓ Groupiness.
            |  ✓ Press button.
            |  ✓ Pull lever.
            |  ✓ Post-conditions
            |    ✓ Button count increased.
            |✓ All pass.
            |Performed 2 actions, 1 check.
          """.stripMargin)

        'beforeAfter - t(actionG addCheck checkPoint.before addCheck checkPoint2.after)(
          """
            |✓ Groupiness.
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✓ Press button.
            |  ✓ Pull lever.
            |  ✓ Post-conditions
            |    ✓ Check more stuff.
            |✓ All pass.
            |Performed 2 actions, 2 checks.
          """.stripMargin)
      }
      'failA1 {
        def a = actionGF1
        'simple - t(a)(
          """
            |✘ Groupiness.
            |  ✘ Press button! -- BUTTON'S BROKEN
            |  - Pull lever.
            |Performed 1 action, 0 checks.
          """.stripMargin)

        'before - t(a addCheck checkPoint.before)(
          """
            |✘ Groupiness.
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✘ Press button! -- BUTTON'S BROKEN
            |  - Pull lever.
            |Performed 1 action, 1 check.
          """.stripMargin)

        'after - t(a addCheck checkPoint.after)(
          """
            |✘ Groupiness.
            |  ✘ Press button! -- BUTTON'S BROKEN
            |  - Pull lever.
            |Performed 1 action, 0 checks.
          """.stripMargin)

        'after - t(a addCheck checkAround)(
          """
            |✘ Groupiness.
            |  ✘ Press button! -- BUTTON'S BROKEN
            |  - Pull lever.
            |Performed 1 action, 0 checks.
          """.stripMargin)

        'beforeAfter - t(a addCheck checkPoint.before addCheck checkPoint2.after)(
          """
            |✘ Groupiness.
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✘ Press button! -- BUTTON'S BROKEN
            |  - Pull lever.
            |Performed 1 action, 1 check.
          """.stripMargin)
      }
      'fail2 {
        def a = actionGF2
        'simple - t(a)(
          """
            |✘ Groupiness.
            |  ✓ Press button.
            |  ✘ Press button! -- BUTTON'S BROKEN
            |Performed 2 actions, 0 checks.
          """.stripMargin)

        'before - t(a addCheck checkPoint.before)(
          """
            |✘ Groupiness.
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✓ Press button.
            |  ✘ Press button! -- BUTTON'S BROKEN
            |Performed 2 actions, 1 check.
          """.stripMargin)

        'after - t(a addCheck checkPoint.after)(
          """
            |✘ Groupiness.
            |  ✓ Press button.
            |  ✘ Press button! -- BUTTON'S BROKEN
            |Performed 2 actions, 0 checks.
          """.stripMargin)

        'around - t(a addCheck checkAround)(
          """
            |✘ Groupiness.
            |  ✓ Press button.
            |  ✘ Press button! -- BUTTON'S BROKEN
            |Performed 2 actions, 0 checks.
          """.stripMargin)

        'beforeAfter - t(a addCheck checkPoint.before addCheck checkPoint2.after)(
          """
            |✘ Groupiness.
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✓ Press button.
            |  ✘ Press button! -- BUTTON'S BROKEN
            |Performed 2 actions, 1 check.
          """.stripMargin)
      }
    }

    'skip {
      'invariant - test(action, checkPoint.skip)(
        """
          |- Initial state.
          |  - Check stuff.
          |✓ Press button.
          |  ✓ Action
          |  - Invariants
          |    - Check stuff.
          |✓ All pass.
          |Performed 1 action, 0 checks.
        """.stripMargin)

      'invariant12 - test(action, checkPoint.skip & checkPoint2)(
        """
          |✓ Initial state.
          |  - Check stuff.
          |  ✓ Check more stuff.
          |✓ Press button.
          |  ✓ Action
          |  ✓ Invariants
          |    - Check stuff.
          |    ✓ Check more stuff.
          |✓ All pass.
          |Performed 1 action, 2 checks.
        """.stripMargin)

      'invariant21 - test(action, checkPoint & checkPoint2.skip)(
        """
          |✓ Initial state.
          |  ✓ Check stuff.
          |  - Check more stuff.
          |✓ Press button.
          |  ✓ Action
          |  ✓ Invariants
          |    ✓ Check stuff.
          |    - Check more stuff.
          |✓ All pass.
          |Performed 1 action, 2 checks.
        """.stripMargin)

      'action - test(actionS, emptyInvariants)(
        """
          |- Press button.
          |- All skipped.
        """.stripMargin)

      'actionBefore - test(action addCheck checkPoint.before.skip, emptyInvariants)(
        """
          |✓ Press button.
          |  - Pre-conditions
          |    - Check stuff.
          |  ✓ Action
          |✓ All pass.
          |Performed 1 action, 0 checks.
        """.stripMargin)

      'actionAround - test(action addCheck checkAround.skip, emptyInvariants)(
        """
          |✓ Press button.
          |  ✓ Action
          |  - Post-conditions
          |    - Button count increased.
          |✓ All pass.
          |Performed 1 action, 0 checks.
        """.stripMargin)

      'actionAfter - test(action addCheck checkPoint.after.skip, emptyInvariants)(
        """
          |✓ Press button.
          |  ✓ Action
          |  - Post-conditions
          |    - Check stuff.
          |✓ All pass.
          |Performed 1 action, 0 checks.
        """.stripMargin)

      'group - test(actionGS, emptyInvariants)(
        """
          |- Groupiness.
          |- All skipped.
        """.stripMargin)

      'group1 - test((actionS >> action2).group("Groupiness."), emptyInvariants)(
        """
          |✓ Groupiness.
          |  - Press button.
          |  ✓ Pull lever.
          |✓ All pass.
          |Performed 1 action, 0 checks.
        """.stripMargin)

      'group2 - test((action >> action2S).group("Groupiness."), emptyInvariants)(
        """
          |✓ Groupiness.
          |  ✓ Press button.
          |  - Pull lever.
          |✓ All pass.
          |Performed 1 action, 0 checks.
        """.stripMargin)

      'group12 - test((actionS >> action2S).group("Groupiness."), emptyInvariants)(
        """
          |- Groupiness.
          |  - Press button.
          |  - Pull lever.
          |- All skipped.
        """.stripMargin)
    }

    'subtest {
      'simple - test(sub1, emptyInvariants)(
        """
          |✓ SubTest!
          |  ✓ Initial state.
          |    ✓ Sub-invariant 1
          |    ✓ Sub-invariant 2
          |  ✓ SubAction!
          |    ✓ Action
          |    ✓ Post-conditions
          |      ✓ Sub around-check
          |    ✓ Invariants
          |      ✓ Sub-invariant 1
          |      ✓ Sub-invariant 2
          |✓ All pass.
          |Performed 1 action, 5 checks.
        """.stripMargin)

      'withInvariants - test(sub1, i12)(
        """
          |✓ Initial state.
          |  ✓ Invariant 1
          |  ✓ Invariant 2
          |✓ SubTest!
          |  ✓ Initial state.
          |    ✓ Sub-invariant 1
          |    ✓ Sub-invariant 2
          |  ✓ SubAction!
          |    ✓ Action
          |    ✓ Post-conditions
          |      ✓ Sub around-check
          |    ✓ Invariants
          |      ✓ Invariant 1
          |      ✓ Invariant 2
          |      ✓ Sub-invariant 1
          |      ✓ Sub-invariant 2
          |✓ All pass.
          |Performed 1 action, 9 checks.
        """.stripMargin)

      'withChecks - test(sub2, emptyInvariants)(
        """
          |✓ SubTest!
          |  ✓ Pre-conditions
          |    ✓ Sub-pre
          |  ✓ Initial state.
          |    ✓ Sub-invariant 1
          |    ✓ Sub-invariant 2
          |  ✓ SubAction!
          |    ✓ Action
          |    ✓ Post-conditions
          |      ✓ Sub around-check
          |    ✓ Invariants
          |      ✓ Sub-invariant 1
          |      ✓ Sub-invariant 2
          |  ✓ Post-conditions
          |    ✓ Sub-delta
          |    ✓ Sub-post
          |✓ All pass.
          |Performed 1 action, 8 checks.
        """.stripMargin)

      'chain - {
        def sub(n: Int, hasAction: Boolean) =
          Plan(
            if (hasAction) mockAction(s"SubAction $n") else *.emptyAction,
            mockPoint(s"Sub #$n invariant")
          ).asAction(s"Sub#$n")

        val t = sub(1, false) >> sub(2, true) >> sub(3, false) >> sub(4, true)
        test(t, mockPoint("Base invariant"))(
          """
            |✓ Initial state.
            |  ✓ Base invariant
            |✓ Sub#1
            |  ✓ Initial state.
            |    ✓ Sub #1 invariant
            |✓ Sub#2
            |  ✓ Initial state.
            |    ✓ Sub #2 invariant
            |  ✓ SubAction 2
            |    ✓ Action
            |    ✓ Invariants
            |      ✓ Base invariant
            |      ✓ Sub #2 invariant
            |✓ Sub#3
            |  ✓ Initial state.
            |    ✓ Sub #3 invariant
            |✓ Sub#4
            |  ✓ Initial state.
            |    ✓ Sub #4 invariant
            |  ✓ SubAction 4
            |    ✓ Action
            |    ✓ Invariants
            |      ✓ Base invariant
            |      ✓ Sub #4 invariant
            |✓ All pass.
            |Performed 2 actions, 9 checks.
          """.stripMargin)
      }

      'nested - {
        def sub(n: String, a: *.Action) =
          Plan(a, mockPoint("Invariant: " + n)).asAction("Subtest: " + n)

        def nest(a0: Boolean, b: Boolean, a2: Boolean) = {
          val n = "" //List(a0,b,a2).map(x => if (x) "1" else "0") mkString ""
          val n1 = n + ".1"
          val n2 = n + ".2"
          var i = sub(n2, if (b) mockAction("Action: " + n2) else *.emptyAction)
          if (a0) i <<= mockAction("Action: " + n1 + "-0")
          if (a2) i >>= mockAction("Action: " + n1 + "-2")
          sub(n1, i)
        }

        val bi = mockPoint("Invariant: Base")

        // Verbose but I want to prove to myself I'm not making a logic error

        "000" - test(nest(false, false, false), bi)(
          """
            |✓ Initial state.
            |  ✓ Invariant: Base
            |✓ Subtest: .1
            |  ✓ Initial state.
            |    ✓ Invariant: .1
            |  ✓ Subtest: .2
            |    ✓ Initial state.
            |      ✓ Invariant: .2
            |✓ All pass.
            |Performed 0 actions, 3 checks.
          """.stripMargin)

        "111" - test(nest(true, true, true), bi)(
          """
            |✓ Initial state.
            |  ✓ Invariant: Base
            |✓ Subtest: .1
            |  ✓ Initial state.
            |    ✓ Invariant: .1
            |  ✓ Action: .1-0
            |    ✓ Action
            |    ✓ Invariants
            |      ✓ Invariant: Base
            |      ✓ Invariant: .1
            |  ✓ Subtest: .2
            |    ✓ Initial state.
            |      ✓ Invariant: .2
            |    ✓ Action: .2
            |      ✓ Action
            |      ✓ Invariants
            |        ✓ Invariant: Base
            |        ✓ Invariant: .1
            |        ✓ Invariant: .2
            |  ✓ Action: .1-2
            |    ✓ Action
            |    ✓ Invariants
            |      ✓ Invariant: Base
            |      ✓ Invariant: .1
            |✓ All pass.
            |Performed 3 actions, 10 checks.
          """.stripMargin)

        "101" - test(nest(true, false, true), bi)(
          """
            |✓ Initial state.
            |  ✓ Invariant: Base
            |✓ Subtest: .1
            |  ✓ Initial state.
            |    ✓ Invariant: .1
            |  ✓ Action: .1-0
            |    ✓ Action
            |    ✓ Invariants
            |      ✓ Invariant: Base
            |      ✓ Invariant: .1
            |  ✓ Subtest: .2
            |    ✓ Initial state.
            |      ✓ Invariant: .2
            |  ✓ Action: .1-2
            |    ✓ Action
            |    ✓ Invariants
            |      ✓ Invariant: Base
            |      ✓ Invariant: .1
            |✓ All pass.
            |Performed 2 actions, 7 checks.
          """.stripMargin)

        "010" - test(nest(false, true, false), bi)(
          """
            |✓ Initial state.
            |  ✓ Invariant: Base
            |✓ Subtest: .1
            |  ✓ Initial state.
            |    ✓ Invariant: .1
            |  ✓ Subtest: .2
            |    ✓ Initial state.
            |      ✓ Invariant: .2
            |    ✓ Action: .2
            |      ✓ Action
            |      ✓ Invariants
            |        ✓ Invariant: Base
            |        ✓ Invariant: .1
            |        ✓ Invariant: .2
            |✓ All pass.
            |Performed 1 action, 6 checks.
          """.stripMargin)

        "011" - test(nest(false, true, true), bi)(
          """
            |✓ Initial state.
            |  ✓ Invariant: Base
            |✓ Subtest: .1
            |  ✓ Initial state.
            |    ✓ Invariant: .1
            |  ✓ Subtest: .2
            |    ✓ Initial state.
            |      ✓ Invariant: .2
            |    ✓ Action: .2
            |      ✓ Action
            |      ✓ Invariants
            |        ✓ Invariant: Base
            |        ✓ Invariant: .1
            |        ✓ Invariant: .2
            |  ✓ Action: .1-2
            |    ✓ Action
            |    ✓ Invariants
            |      ✓ Invariant: Base
            |      ✓ Invariant: .1
            |✓ All pass.
            |Performed 2 actions, 8 checks.
          """.stripMargin)

        "100" - test(nest(true, false, false), bi)(
          """
            |✓ Initial state.
            |  ✓ Invariant: Base
            |✓ Subtest: .1
            |  ✓ Initial state.
            |    ✓ Invariant: .1
            |  ✓ Action: .1-0
            |    ✓ Action
            |    ✓ Invariants
            |      ✓ Invariant: Base
            |      ✓ Invariant: .1
            |  ✓ Subtest: .2
            |    ✓ Initial state.
            |      ✓ Invariant: .2
            |✓ All pass.
            |Performed 1 action, 5 checks.
          """.stripMargin)

        "001" - test(nest(false, false, true), bi)(
          """
            |✓ Initial state.
            |  ✓ Invariant: Base
            |✓ Subtest: .1
            |  ✓ Initial state.
            |    ✓ Invariant: .1
            |  ✓ Subtest: .2
            |    ✓ Initial state.
            |      ✓ Invariant: .2
            |  ✓ Action: .1-2
            |    ✓ Action
            |    ✓ Invariants
            |      ✓ Invariant: Base
            |      ✓ Invariant: .1
            |✓ All pass.
            |Performed 1 action, 5 checks.
          """.stripMargin)

        "110" - test(nest(true, true, false), bi)(
          """
            |✓ Initial state.
            |  ✓ Invariant: Base
            |✓ Subtest: .1
            |  ✓ Initial state.
            |    ✓ Invariant: .1
            |  ✓ Action: .1-0
            |    ✓ Action
            |    ✓ Invariants
            |      ✓ Invariant: Base
            |      ✓ Invariant: .1
            |  ✓ Subtest: .2
            |    ✓ Initial state.
            |      ✓ Invariant: .2
            |    ✓ Action: .2
            |      ✓ Action
            |      ✓ Invariants
            |        ✓ Invariant: Base
            |        ✓ Invariant: .1
            |        ✓ Invariant: .2
            |✓ All pass.
            |Performed 2 actions, 8 checks.
          """.stripMargin)
      }

      'singleActionName {
        val t = Plan.withoutInvariants(action addCheck checkPoint2.before).asAction("SubTest!")
        test(t, checkPoint)(
          """
            |✓ Initial state.
            |  ✓ Check stuff.
            |✓ SubTest!
            |  ✓ Press button.
            |    ✓ Pre-conditions
            |      ✓ Check more stuff.
            |    ✓ Action
            |    ✓ Invariants
            |      ✓ Check stuff.
            |✓ All pass.
            |Performed 1 action, 3 checks.
          """.stripMargin)
      }
    }

    'bulk {
      def set(n: String) =
        mockAction(n)
          .addCheck(mockPoint ("Pre "    + n + 1).before)
          .addCheck(mockPoint ("Pre "    + n + 2).before)
          .addCheck(mockPoint ("Post "   + n + 1).after)
          .addCheck(mockPoint ("Post "   + n + 2).after)
          .addCheck(mockAround("Around " + n + 1))
          .addCheck(mockAround("Around " + n + 2))
      val a =
        set("A") >>
        (set("B") >> set("C").skip >> set("D")).group("B ~ D") >>
        (set("E") >> set("F")).group("E & F").skip >>
        set("G")

      val all = sub1 >> a >> sub2

      test(all, i12)(
        """
          |✓ Initial state.
          |  ✓ Invariant 1
          |  ✓ Invariant 2
          |✓ SubTest!
          |  ✓ Initial state.
          |    ✓ Sub-invariant 1
          |    ✓ Sub-invariant 2
          |  ✓ SubAction!
          |    ✓ Action
          |    ✓ Post-conditions
          |      ✓ Sub around-check
          |    ✓ Invariants
          |      ✓ Invariant 1
          |      ✓ Invariant 2
          |      ✓ Sub-invariant 1
          |      ✓ Sub-invariant 2
          |✓ A
          |  ✓ Pre-conditions
          |    ✓ Pre A1
          |    ✓ Pre A2
          |  ✓ Action
          |  ✓ Post-conditions
          |    ✓ Around A1
          |    ✓ Around A2
          |    ✓ Post A1
          |    ✓ Post A2
          |  ✓ Invariants
          |    ✓ Invariant 1
          |    ✓ Invariant 2
          |✓ B ~ D
          |  ✓ B
          |    ✓ Pre-conditions
          |      ✓ Pre B1
          |      ✓ Pre B2
          |    ✓ Action
          |    ✓ Post-conditions
          |      ✓ Around B1
          |      ✓ Around B2
          |      ✓ Post B1
          |      ✓ Post B2
          |    ✓ Invariants
          |      ✓ Invariant 1
          |      ✓ Invariant 2
          |  - C
          |  ✓ D
          |    ✓ Pre-conditions
          |      ✓ Pre D1
          |      ✓ Pre D2
          |    ✓ Action
          |    ✓ Post-conditions
          |      ✓ Around D1
          |      ✓ Around D2
          |      ✓ Post D1
          |      ✓ Post D2
          |    ✓ Invariants
          |      ✓ Invariant 1
          |      ✓ Invariant 2
          |  ✓ Invariants
          |    ✓ Invariant 1
          |    ✓ Invariant 2
          |- E & F
          |✓ G
          |  ✓ Pre-conditions
          |    ✓ Pre G1
          |    ✓ Pre G2
          |  ✓ Action
          |  ✓ Post-conditions
          |    ✓ Around G1
          |    ✓ Around G2
          |    ✓ Post G1
          |    ✓ Post G2
          |  ✓ Invariants
          |    ✓ Invariant 1
          |    ✓ Invariant 2
          |✓ SubTest!
          |  ✓ Pre-conditions
          |    ✓ Sub-pre
          |  ✓ Initial state.
          |    ✓ Sub-invariant 1
          |    ✓ Sub-invariant 2
          |  ✓ SubAction!
          |    ✓ Action
          |    ✓ Post-conditions
          |      ✓ Sub around-check
          |    ✓ Invariants
          |      ✓ Invariant 1
          |      ✓ Invariant 2
          |      ✓ Sub-invariant 1
          |      ✓ Sub-invariant 2
          |  ✓ Post-conditions
          |    ✓ Sub-delta
          |    ✓ Sub-post
          |✓ All pass.
          |Performed 6 actions, 53 checks.
        """.stripMargin)
    }

    'duplicates {
      'invariants {
        test(action, checkPoint & checkPoint)(
          """
            |✓ Initial state.
            |  ✓ Check stuff.
            |✓ Press button.
            |  ✓ Action
            |  ✓ Invariants
            |    ✓ Check stuff.
            |✓ All pass.
            |Performed 1 action, 2 checks.
          """.stripMargin)
      }
      'delta {
        test(action.addCheck(checkAround).addCheck(checkAround), emptyInvariants)(
          """
            |✓ Press button.
            |  ✓ Action
            |  ✓ Post-conditions
            |    ✓ Button count increased.
            |✓ All pass.
            |Performed 1 action, 1 check.
          """.stripMargin)
      }
    }

    'obsAndState {
      'pass - test(action addCheck checkOS.after, emptyInvariants)(
        """
          |✓ Press button.
          |  ✓ Action
          |  ✓ Post-conditions
          |    ✓ Hurp should be 1.
          |✓ All pass.
          |Performed 1 action, 1 check.
        """.stripMargin)

      'fail - {
        // These CCs ensure correctness via .observe and .run
        case class State(s: Int)
        case class Obs(o: Int)
        val * = Dsl[Unit, Obs, State]
        val checkOSFail = *.focus("Evil").obsAndState(_.o, _.s).assert.equal
        val action = *.action("Press button.").act(_ => ())
        val r = Plan.withoutInvariants(action addCheck checkOSFail.after)
          .test(Observer watch Obs(777))
          .runU(State(666))
        assertRun(r,
          """
            |✘ Press button.
            |  ✓ Action
            |  ✘ Post-conditions
            |    ✘ Evil should be 666. -- Expected 666, not 777.
            |Performed 1 action, 1 check.
          """.stripMargin)
      }
    }

    'existenceOfAround {
      var is = List(1, 2, 3)
      val * = Dsl[Unit, List[Int], Boolean]
      val a = *.action("Remove 2").act(_ => is = List(1, 3)).updateState(_ => false)
      val c = *.focus("X").collection(_.obs).assert.existenceOf(2)(_.state)
      val r = Plan.withoutInvariants(a addCheck c.beforeAndAfter).test(Observer watch is).runU(true)
      assertRun(r,
        """
          |✓ Remove 2
          |  ✓ Pre-conditions
          |    ✓ X should contain 2.
          |  ✓ Action
          |  ✓ Post-conditions
          |    ✓ X shouldn't contain 2.
          |✓ All pass.
          |Performed 1 action, 2 checks.
        """.stripMargin)
    }

    // action combinators

  }
}
