package teststate.example.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import monocle.macros.GenLens

object TodoComponent {

  case class TodoItem(text: String, completed: Boolean)

  object TodoItem {
    val completed = GenLens[TodoItem](_.completed)
  }

  case class State(newItemText  : String,
                   items        : Vector[TodoItem],
                   showCompleted: Boolean) {

    def hideCompleted = !showCompleted
  }

  object State {
    val newItemText   = GenLens[State](_.newItemText)
    val items         = GenLens[State](_.items)
    val showCompleted = GenLens[State](_.showCompleted)

    def item(idx: Int): Lens[State, TodoItem] =
      items andThen atVectorIndex(idx)
  }

  def atVectorIndex[A](idx: Int) =
    Lens[Vector[A], A](_ apply idx)(a => _.updated(idx, a))

  private val newFormMarker =
    VdomAttr.devOnly("data-new-form") := 1

  final class Backend($: BackendScope[Unit, State]) {

    def render(s: State): VdomElement = {

      val items =
        if (s.showCompleted)
          s.items
        else
          s.items.iterator.filterNot(_.completed)

      <.div(
        renderNew(s.newItemText),
        renderList(items),
        renderToggleShowCompleted(s.showCompleted),
        renderSummary(s.items))
    }

    private def renderNew(text: String) = {
      // Prevent saving blank todos
      val trimmed: Option[String] =
        Some(text.trim).filter(_.nonEmpty)

      val save: Option[Callback] =
        trimmed map createNewItem

      <.div(
        newFormMarker,
        <.input.text(
          ^.value := text,
          ^.onChange ==> updateNewText),
        <.button(
          ^.`type` := "submit",
          ^.disabled := save.isEmpty,
          ^.onClick -->? save,
          "Add"))
    }

    private def renderList(items: IterableOnce[TodoItem]): TagMod =
      if (items.iterator.isEmpty)
        EmptyVdom
      else
        <.div(
          "Todo List",
          <.ul(
            items.iterator.zipWithIndex.map { case (item, idx) =>
              <.li(
                ^.key := idx,
                renderItem(item, idx))
            }.toVdomArray))

    private def renderItem(item: TodoItem, idx: Int) = {
      val complete =
        if (item.completed)
          EmptyVdom
        else
          <.button(
            ^.onClick --> completeItem(idx),
            "Complete")

      <.div(
        <.pre(item.text),
        complete)
    }

    private def renderToggleShowCompleted(showCompleted: Boolean) =
      <.label(
        <.input.checkbox(
          ^.checked := showCompleted,
          ^.onChange --> $.modState(State.showCompleted replace !showCompleted)),
        "Show completed.")

    private def renderSummary(items: Vector[TodoItem]) = {
      val total    = items.length
      val complete = items.count(_.completed)
      val pending  = total - complete

      def row(title: String, n: Int) =
        <.tr(
          <.td(title + ":"),
          <.td(n))

      <.table(
        <.tbody(
          row("Complete", complete),
          row("Pending" , pending),
          row("Total"   , total)))
    }

    private def updateNewText(ev: ReactEventFromInput): Callback =
      ev.extract(_.target.value)(text =>
        $.modState(State.newItemText replace text))

    private def createNewItem(text: String): Callback =
      $.modState(
        State.newItemText.replace("") compose
        State.items.modify(_ :+ TodoItem(text, false)))

    private def completeItem(idx: Int): Callback =
      $.modState(
        (State.item(idx) andThen TodoItem.completed) replace true)
  }

  val Component = ScalaComponent.builder[Unit]("Todo Example")
    .initialState(State("", Vector.empty, false))
    .renderBackend[Backend]
    .build
}
