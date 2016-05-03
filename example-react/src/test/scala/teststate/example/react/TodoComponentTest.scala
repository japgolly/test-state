package teststate.example.react

import japgolly.scalajs.react.test._
import utest._
import TestState._

object TodoComponentTest extends TestSuite {
  import TodoTestDSL._

  /**
   * Here be all the invariants for the TodoComponent.
   *
   * They be validated before and after each action executes, arr.
   */
  val invariants: *.Invariant = {
    var invars = *.emptyInvariant

    // Invariant #1
    invars &= *.focus("Summary: total and (complete + pending)")
      .compare(_.obs.summaryTotal, i => i.obs.summaryComplete + i.obs.summaryPending)
      .assert.equal

    // Invariant #2
    val itemCountEqualsTotal   = itemCount.assert.equalBy(_.obs.summaryTotal)
    val itemCountEqualsPending = itemCount.assert.equalBy(_.obs.summaryPending)
    invars &= *.chooseInvariant("Expected visible items")(_.obs.showingComplete match {
      case true  => itemCountEqualsTotal
      case false => itemCountEqualsPending
    })

    // Invariant #3
    invars &= *.focus("Number of 'Complete' buttons")
      .compare(_.obs.completeItemButtonCount, _.obs.summaryPending)
      .assert.equal

    // Invariant #4
    invars &= *.test("NewItem: Blank text = disabled 'Add' button")(i =>
      i.obs.newItemText.trim.isEmpty == i.obs.newItemButtonDisabled)

    // Ensure observation always matches expected state
    //
    // (The only reason this doesn't start with "invars &= " like the invariants above, is simply to demonstrate that
    // invariants are just normal immutable Scala values. You can compose with & whenever and however you like).
    //
    val expectedStateIsCorrect =
      *.focus("Total items"    ).obsAndState(_.summaryTotal   , _.total    ).assert.equal &
      *.focus("Completed items").obsAndState(_.summaryComplete, _.completed).assert.equal

    invars & expectedStateIsCorrect
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def runTest(plan: *.Plan): Report[String] =
    ReactTestUtils.withRenderedIntoDocument(TodoComponent.Component()) { c =>

      def observe() = new TodoObs(c.htmlDomZipper)

      val test = plan
        .addInvariants(invariants)
        .withInitialState(State(0, 0))
        .test(Observer watch observe())

      test.runU
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  override def tests = TestSuite {

    val plan = Plan.action(
      addItem("hello")
      >> addItem("hello2")
      >> addItem(" blah 3 ")    +> visibleItemNames.assert.contains("blah 3")
      >> completeItem("hello2") +> visibleItemNames.size.assert.decrement
      >> toggleShowCompleted    +> visibleItemNames.size.assert.increment
      >> toggleShowCompleted    +> visibleItemNames.assert("hello", "blah 3")
    )

    runTest(plan).assert()
  }
}
