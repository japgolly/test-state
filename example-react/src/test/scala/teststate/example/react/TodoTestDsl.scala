package teststate.example.react

import japgolly.scalajs.react.test._
import monocle.macros.Lenses
import org.scalajs.dom.html
import teststate.example.react.MyTestState._

/**
 * Observation capturing everything we want to know about the Todo component.
 *
 * This becomes the stateless snapshot of the component's DOM / what a user would see.
 */
final class TodoObs($: DomZipperJs) {

  val showCompleteInput =
    $("label:contains('Show') input").domAs[html.Input]

  val showingComplete: Boolean =
    showCompleteInput.checked

  private val newForm = $("*[data-new-form]")

  val newItemInput  = newForm("input" ).domAs[html.Input ]
  val newItemButton = newForm("button").domAs[html.Button]

  val newItemText: String =
    newItemInput.value

  val newItemButtonDisabled: Boolean =
    newItemButton.disabled

  private val items =
    $.collect0n("li")

  val itemCompleteButtons: Map[String, Option[html.Button]] =
    items.map(z =>
      z("pre").innerText -> z.collect01("button").domsAs[html.Button]
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

object TodoTestDsl {

  /**
   * This is some (expected) state we will maintain ourselves as actions are performed.
   *
   * It is used to make ensure that observations about the real state (via [[TodoObs]]) are what we expect.
   */
  @Lenses
  case class State(total: Int, completed: Int)

  val dsl = Dsl[Unit, TodoObs, State]

  val showingComplete  = dsl.focus("ShowingComplete"   ).value(_.obs.showingComplete)
  val itemCount        = dsl.focus("Visible item count").value(_.obs.visibleItemCount)
  val visibleItemNames = dsl.focus("Visible item names").collection(_.obs.itemCompleteButtons.keys)

  def setNewText(text: String): dsl.Actions =
    dsl.action(s"Set new text to '$text'")(SimEvent.Change(text) simulate _.obs.newItemInput)

  val clickAdd: dsl.Actions =
    dsl.action("Click the 'Add' button")(Simulate click _.obs.newItemButton)

  def addItem(text: String): dsl.Actions =
    ( setNewText(text)
      +> dsl.test("Add button must be enabled.")(!_.obs.newItemButtonDisabled)
      >> clickAdd.updateState(State.total.modify(_ + 1))
      +> dsl.focus("New item text").value(_.obs.newItemText).assert("")
    ).group(s"Add item: '$text'")

  def completeItem(name: String): dsl.Actions =
    dsl.action("Complete item: " + name).attempt(_.obs.itemCompleteButtons.get(name) match {
      case None          => Some("Item not found.")
      case Some(None)    => Some("Item missing 'Complete' button.")
      case Some(Some(b)) => Simulate click b; None
    })
    .updateState(State.completed.modify(_ + 1))

  val toggleShowCompleted: dsl.Actions =
    dsl.action("Toggle ShowCompleted")(Simulate change _.obs.showCompleteInput) +>
      showingComplete.assert.change
}

