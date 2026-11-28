# ScalaJsReact Example

- [Concept](#concept)
- Walkthrough
  - [Test Setup](#test-setup)
  - [Invariants](#invariants)
  - [Actions](#actions)
  - [Running](#running)
- [Sample Output](#sample-output)


## Concept
In this example, there is a [scalajs-react](https://github.com/japgolly/scalajs-react)
component called [`TodoComponent`](src/main/scala/teststate/example/react/TodoComponent.scala).

It renders a view like this:
```
[_________________] [Add]

Todo List
=========

* Do washing. [Complete]
* Buy milk.
* Buy bread.  [Complete]
* Make toast. [Complete]

[X] Show completed.

Complete: 1
Pending : 3
Total   : 4
```

The idea is that:
* at the top, you enter a task and click `Add`, which sends it to the Todo List.
* Tasks in the Todo List have a `Complete` button beside them which you use to make them as done.
* Completed tasks can be hidden using the checkbox.
* The summary at the bottom is always up-to-date.

## Test Setup

Here we want to use TestState itself (`core`), the scalajs-react extensions, and the DomZipper.
We don't want to have to think about *how* DomZipper will query DOM so we will use the default `dom-zipper-sizzle` bundle which takes care of it.

Firstly we create a `TestState` object for the project that combines our dependencies.
<br>See [MyTestState.scala](src/test/scala/teststate/example/react/MyTestState.scala).

Next we create a class to observe the component.
```scala
class TodoObs($: HtmlDomZipper)
```

And we create a DSL and put it somewhere.
Let's also keep track of what we expect the number of completed and total items will be.
```scala
object TodoTestDsl {

  case class State(total: Int, completed: Int)

  val dsl = Dsl[Unit, TodoObs, State]
}
```

Now we're set to start writing tests.

## Invariants

Let's start with an invariant: `Total` should always equal `Complete` + `Pending`.

Firstly, let's capture the numbers on the screen (i.e. the component's output).
There are a number of ways to do this, an easy way is by CSS selector expressions.

We know there's only one `<table>` in the output, and that the numbers are in the 2nd `<td>`.
And so we write this into our observation.
```scala
class TodoObs($: HtmlDomZipper) {

  // There is only one <table>
  private val summaryTable = $("table")

  // Find the row first, then get its 2nd <td>
  private def summaryInt(name: String): Int =
    summaryTable(s"tr:contains('$name')")("td", 2 of 2).innerText.toInt

  val total    = summaryInt("Total")
  val complete = summaryInt("Complete")
  val pending  = summaryInt("Pending")
}
```

And then we write the invariant:
```scala
dsl.focus("Summary total").value(_.obs.total)
  .assert.equalBy(i => i.obs.complete + i.obs.pending)
```

Now let's ensure that the observed total always matches our expected total in our `State` class.

```scala
dsl.focus("Total items").obsAndState(_.total, _.total).assert.equal
//                          TodoObs ↗         ↖ State
```

## Actions

Let's write an action to toggle the `Show completed` checkbox.
First we capture the checkbox DOM,
```scala
import org.scalajs.dom.html

class TodoObs($: HtmlDomZipper) {

  val showCompleteInput =
    $("label:contains('Show') input").domAs[html.Input]

  // ...
}
```

Then we create an action around it.
```scala
import japgolly.scalajs.react.test.ReactTestUtils.Simulate

val toggleShowCompleted =
  dsl.action("Toggle ShowCompleted")(Simulate change _.obs.showCompleteInput)
```

Now let's make sure that whenever it's invoked, it ensures that the checkbox changes.
First collect the value:
```scala
class TodoObs($: HtmlDomZipper) {

  val showCompleteInput =
    $("label:contains('Show') input").domAs[html.Input]

  val showingComplete: Boolean =
    showCompleteInput.checked

  // ...
}
```
then update our test dsl.
```scala
val showingComplete = dsl.focus("Showing Complete").value(_.obs.showingComplete)

val toggleShowCompleted =
  dsl.action("Toggle ShowCompleted")(Simulate change _.obs.showCompleteInput) +>
  showingComplete.assert.change
```

## Running

As for how to glue everything together and run tests,
it's best to just have a look [at the source](src/test/scala/teststate/example/react/) yourself.
It's short.

You can also clone this repo and run the tests via:
```sh
sbt exampleReactJS/test
```


# Sample Output

Here is the code for the [test in this example](src/test/scala/teststate/example/react/TodoTest.scala).

```scala
  addItem("hello")
  >> addItem("hello2")
  >> addItem(" blah 3 ")    +> visibleItemNames.assert.contains("blah 3")
  >> completeItem("hello2") +> visibleItemNames.size.assert.decrement
  >> toggleShowCompleted    +> visibleItemNames.size.assert.increment
  >> toggleShowCompleted    +> visibleItemNames.assert("hello", "blah 3")
```

This is what you see (by default) when the test passes.

![pass](output-pass.png)

This is an example of test failure.

![fail](output-fail.png)

You can also tell TestState to show you everything it does in glorious and excruciating detail, if you so wish.

![verbose](output-verbose.png)
