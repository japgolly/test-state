package teststate.example.react

import monocle.macros.Lenses
import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import ReactTestUtils.Simulate

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object TestState
  extends teststate.Exports
     with teststate.TestStateReact
     with teststate.domzipper.sizzle.Exports

import TestState._

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Observes everything we want to know about the Todo component.
 *
 * This becomes the stateless snapshot of the component's DOM / what a user would see.
 */
final class TodoObs($: HtmlDomZipper) {

  val showCompleteInput =
    $("label:contains('Show') input").domAs[html.Input]

  val showingComplete: Boolean =
    showCompleteInput.checked

  private val newForm = $("*[data-new-form]")

  val newItemInput  = newForm("input" ).as[html.Input ].dom
  val newItemButton = newForm("button").as[html.Button].dom

  val newItemText: String =
    newItemInput.value

  val newItemButtonDisabled: Boolean =
    newItemButton.disabled.get

  private val items =
    $.collect0n("li")

  val itemCompleteButtons: Map[String, Option[html.Button]] =
    items.mapZippers(z =>
      z("pre").innerText -> z.collect01("button").as[html.Button].doms
    ).toMap

  val visibleItemCount: Int =
    items.size

  val completeItemButtonCount: Int =
    itemCompleteButtons.valuesIterator.count(_.isDefined)

  // There is only one <table>
  private val summaryTable = $("table")

  // Find the row first, then get its 2nd <td>
  private def summaryInt(name: String): Int =
    summaryTable(s"tr:contains('$name')")("td", 2 of 2).innerText.toInt

  val summaryTotal    = summaryInt("Total")
  val summaryComplete = summaryInt("Complete")
  val summaryPending  = summaryInt("Pending")
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
  *
  */
object TodoTestDSL {

  @Lenses
  case class State(total: Int, completed: Int)

  val * = Dsl[Unit, TodoObs, State]

  val showingComplete = *.focus("ShowComplete").value(_.obs.showingComplete)

  val itemCount = *.focus("Number of items").value(_.obs.visibleItemCount)

  val visibleItemNames = *.focus("Visible item names").collection(_.obs.itemCompleteButtons.keys)

  def setNewText(text: String): *.Action =
    *.action(s"Set new text to '$text'")(ChangeEventData(text) simulate _.obs.newItemInput)

  val clickAdd: *.Action =
    *.action("Click the 'Add' button")(Simulate click _.obs.newItemButton)

  def addItem(text: String): *.Action =
    ( setNewText(text)
      +> *.test("Add button must be enabled.")(!_.obs.newItemButtonDisabled)
      >> clickAdd.updateState(State.total.modify(_ + 1))
      +> *.focus("New item text").value(_.obs.newItemText).assert("")
    ).group(s"Add item: '$text'")

  def completeItem(name: String): *.Action =
    *.action("Complete item: " + name).attempt(_.obs.itemCompleteButtons.get(name) match {
      case None          => Some("Item not found.")
      case Some(None)    => Some("Item missing 'Complete' button.")
      case Some(Some(b)) => Simulate click b; None
    })
    .updateState(State.completed.modify(_ + 1))

  val toggleShowCompleted: *.Action =
    *.action("Toggle ShowCompleted")(Simulate change _.obs.showCompleteInput) +>
      showingComplete.assert.change
}

