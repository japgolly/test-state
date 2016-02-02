package teststate

import utest._
import TestUtil._

object OutputTest extends TestSuite {

  val * = Dsl.sync[Unit, Unit, Unit, String]

  val options = History.Options.uncolored.alwaysShowChildren

  def mockAction(name: String) = *.action(name).act(_ => ())
  def mockPoint (name: String) = *.point(name, _ => None)
  def mockAround(name: String) = *.around(name, _ => ())((_, _) => None)

  val action    = mockAction("Press button.")
  val action2   = mockAction("Pull lever.")
  val actionF   = *.action("Press button!").actTry(_ => Some("BUTTON'S BROKEN"))
  val actionG   = (action >> action2).group("Groupiness.")
  val actionGF1 = (actionF >> action2).group("Groupiness.")
  val actionGF2 = (action >> actionF).group("Groupiness.")
  val actionGS  = actionG.when(_ => false)
  val actionS   = action.when(_ => false)
  val action2S  = action2.when(_ => false)

  val checkPoint   = mockPoint("Check stuff.")
  val checkPoint2  = mockPoint("Check more stuff.")
  val checkPointF  = *.point("Check failure.", _ => Some("Shit broke!"))
  val checkAround  = mockAround("Button count increased.")
  val checkAroundF = *.around("Button count increased.", _ => ())((_, _) => Some("2 != 3"))

  def test(a: *.Action, i: *.Check)(expect: String): Unit = {
    val h = Test(a, i).observe(_ => ()).run((), ())
    val actual = h.format(options).trim
    assertEq(actual = actual, expect.trim)
  }

  override def tests = TestSuite {

    'empty - test(Action.empty, Check.empty)("- Nothing to do.")

    'invariants {
      def t(i: *.Check)(expect: String) = test(Action.empty, i)(expect)
      'pass {
        'simplest - t(checkPoint)(
          """
            |✓ Initial state.
            |  ✓ Check stuff.
            |✓ All pass.
          """.stripMargin)

        'multiple - t(checkPoint & checkPoint2)(
          """
            |✓ Initial state.
            |  ✓ Check stuff.
            |  ✓ Check more stuff.
            |✓ All pass.
          """.stripMargin)
      }
      'fail {
        'single - t(checkPointF)(
          """
            |✘ Initial state.
            |  ✘ Check failure. -- Shit broke!
          """.stripMargin)

        'first - t(checkPointF & checkPoint2)(
          """
            |✘ Initial state.
            |  ✘ Check failure. -- Shit broke!
            |  ✓ Check more stuff.
          """.stripMargin)

        'second - t(checkPoint & checkPointF)(
          """
            |✘ Initial state.
            |  ✓ Check stuff.
            |  ✘ Check failure. -- Shit broke!
          """.stripMargin)
      }
    }

    'action {
      def t(a: *.Action)(expect: String) = test(a, Check.empty)(expect)
      'pass {
        'simplest - t(action)(
          """
            |✓ Press button.
            |✓ All pass.
          """.stripMargin)

        'before - t(action addCheck checkPoint.before)(
          """
            |✓ Press button.
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✓ Action
            |✓ All pass.
          """.stripMargin)

        'after - t(action addCheck checkPoint.after)(
          """
            |✓ Press button.
            |  ✓ Action
            |  ✓ Post-conditions
            |    ✓ Check stuff.
            |✓ All pass.
          """.stripMargin)

        'around - t(action addCheck checkAround)(
          """
            |✓ Press button.
            |  ✓ Action
            |  ✓ Post-conditions
            |    ✓ Button count increased.
            |✓ All pass.
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
          """.stripMargin)
      }
      'fail {
        'simplest - t(actionF)(
          """
            |✘ Press button! -- BUTTON'S BROKEN
          """.stripMargin)

        'beforeA - t(actionF addCheck checkPoint.before)(
          """
            |✘ Press button!
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✘ Action -- BUTTON'S BROKEN
          """.stripMargin)

        'beforeC - t(action addCheck checkPointF.before)(
          """
            |✘ Press button.
            |  ✘ Pre-conditions
            |    ✘ Check failure. -- Shit broke!
          """.stripMargin)

        'afterA - t(actionF addCheck checkPoint.after)(
          """
            |✘ Press button! -- BUTTON'S BROKEN
          """.stripMargin)

        'afterC - t(action addCheck checkPointF.after)(
          """
            |✘ Press button.
            |  ✓ Action
            |  ✘ Post-conditions
            |    ✘ Check failure. -- Shit broke!
          """.stripMargin)

        'around - t(action addCheck checkAroundF)(
          """
            |✘ Press button.
            |  ✓ Action
            |  ✘ Post-conditions
            |    ✘ Button count increased. -- 2 != 3
          """.stripMargin)

        'beforeAfter1 - t(action addCheck checkPoint.after addCheck checkPointF.before)(
          """
            |✘ Press button.
            |  ✘ Pre-conditions
            |    ✘ Check failure. -- Shit broke!
          """.stripMargin)

        'beforeAfter2 - t(actionF addCheck checkPoint.after addCheck checkPoint2.before)(
          """
            |✘ Press button!
            |  ✓ Pre-conditions
            |    ✓ Check more stuff.
            |  ✘ Action -- BUTTON'S BROKEN
          """.stripMargin)

        'beforeAfter3 - t(action addCheck checkPointF.after addCheck checkPoint2.before)(
          """
            |✘ Press button.
            |  ✓ Pre-conditions
            |    ✓ Check more stuff.
            |  ✓ Action
            |  ✘ Post-conditions
            |    ✘ Check failure. -- Shit broke!
          """.stripMargin)
      }
    }

    'actionG {
      def t(a: *.Action)(expect: String) = test(a, Check.empty)(expect)
      'pass {
        'simple - t(actionG)(
          """
            |✓ Groupiness.
            |  ✓ Press button.
            |  ✓ Pull lever.
            |✓ All pass.
          """.stripMargin)

        'before - t(actionG addCheck checkPoint.before)(
          """
            |✓ Groupiness.
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✓ Press button.
            |  ✓ Pull lever.
            |✓ All pass.
          """.stripMargin)

        'after - t(actionG addCheck checkPoint.after)(
          """
            |✓ Groupiness.
            |  ✓ Press button.
            |  ✓ Pull lever.
            |  ✓ Post-conditions
            |    ✓ Check stuff.
            |✓ All pass.
          """.stripMargin)

        'around - t(actionG addCheck checkAround)(
          """
            |✓ Groupiness.
            |  ✓ Press button.
            |  ✓ Pull lever.
            |  ✓ Post-conditions
            |    ✓ Button count increased.
            |✓ All pass.
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
          """.stripMargin)
      }
      'failA1 {
        def a = actionGF1
        'simple - t(a)(
          """
            |✘ Groupiness.
            |  ✘ Press button! -- BUTTON'S BROKEN
          """.stripMargin)

        'before - t(a addCheck checkPoint.before)(
          """
            |✘ Groupiness.
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✘ Press button! -- BUTTON'S BROKEN
          """.stripMargin)

        'after - t(a addCheck checkPoint.after)(
          """
            |✘ Groupiness.
            |  ✘ Press button! -- BUTTON'S BROKEN
          """.stripMargin)

        'after - t(a addCheck checkAround)(
          """
            |✘ Groupiness.
            |  ✘ Press button! -- BUTTON'S BROKEN
          """.stripMargin)

        'beforeAfter - t(a addCheck checkPoint.before addCheck checkPoint2.after)(
          """
            |✘ Groupiness.
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✘ Press button! -- BUTTON'S BROKEN
          """.stripMargin)
      }
      'fail2 {
        def a = actionGF2
        'simple - t(a)(
          """
            |✘ Groupiness.
            |  ✓ Press button.
            |  ✘ Press button! -- BUTTON'S BROKEN
          """.stripMargin)

        'before - t(a addCheck checkPoint.before)(
          """
            |✘ Groupiness.
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✓ Press button.
            |  ✘ Press button! -- BUTTON'S BROKEN
          """.stripMargin)

        'after - t(a addCheck checkPoint.after)(
          """
            |✘ Groupiness.
            |  ✓ Press button.
            |  ✘ Press button! -- BUTTON'S BROKEN
          """.stripMargin)

        'around - t(a addCheck checkAround)(
          """
            |✘ Groupiness.
            |  ✓ Press button.
            |  ✘ Press button! -- BUTTON'S BROKEN
          """.stripMargin)

        'beforeAfter - t(a addCheck checkPoint.before addCheck checkPoint2.after)(
          """
            |✘ Groupiness.
            |  ✓ Pre-conditions
            |    ✓ Check stuff.
            |  ✓ Press button.
            |  ✘ Press button! -- BUTTON'S BROKEN
          """.stripMargin)
      }
    }

    'skip {
      'simple - test(actionS, Check.empty)(
        """
          |- Press button.
          |- All skipped.
        """.stripMargin)

      'group - test(actionGS, Check.empty)(
        """
          |- Groupiness.
          |- All skipped.
        """.stripMargin)

      'group1 - test((actionS >> action2).group("Groupiness."), Check.empty)(
        """
          |✓ Groupiness.
          |  - Press button.
          |  ✓ Pull lever.
          |✓ All pass.
        """.stripMargin)

      'group2 - test((action >> action2S).group("Groupiness."), Check.empty)(
        """
          |✓ Groupiness.
          |  ✓ Press button.
          |  - Pull lever.
          |✓ All pass.
        """.stripMargin)

      'group12 - test((actionS >> action2S).group("Groupiness."), Check.empty)(
        """
          |- Groupiness.
          |  - Press button.
          |  - Pull lever.
          |- All skipped.
        """.stripMargin)
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
      val i = mockPoint("Invariant 1") & mockPoint("Invariant 2")
      val a =
        set("A") >>
        (set("B") >> set("C").when(_ => false) >> set("D")).group("B ~ D") >>
        (set("E") >> set("F")).group("E & F").when(_ => false) >>
        set("G")

      test(a, i)(
        """
          |✓ Initial state.
          |  ✓ Invariant 1
          |  ✓ Invariant 2
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
          |✓ All pass.
        """.stripMargin)
    }

    // action combinators

  }
}
