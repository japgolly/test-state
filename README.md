# Testate

test   
||||
||state
|||||||
testate
"test state"

### Foreword

Are you sick of internet elitists in their ivory towers going on and on about how inferior FP is and how much more productive it is to code imperatively?
If you're like me, you live in, and code for the Real World.
We don't live a vaccuum where we can just write effects everywhere and spend all day admiring how readable it is "in theory".
That might be fine for PMs but we have jobs to do. We just want to get work done.
We're pragmatic and responsible. That's why we use strongly-typed FP and embrace statelessness.

Sure there are times where ivory-tower stateful style and effects are useful.
When you want to test it in a pragmatic stateless, composable, and intelligible way,
well that's what this library exists to facilitate.


### What this IS

This is a library to bring all those nice things we care about when testing stateless code
to testing stateful code.

* unit testing a webapp with Scala.JS.
* integration testing a webapp.
* testing DB state or a DB migration.
* random-testing (fuzz-testing) like Android's monkeyrunner


* Scala & Scala.JS.
* Sync & Async (stack-safe).
* Test stateful, effectful code & stateless pure core.
* Property test (∀) & imperatively test (∃).
* Products & coproducts.



### What this IS *NOT*

This is not a test framework.
<br>It is not a replacement for ScalaTest, Specs2, μTest, etc.
<br>It is to be used in conjunction with your favourite test framework.

This is not a property testing library.
<br>It is not a replacement for ScalaCheck, Nyaya, ScalaProps.
<br>It is to be used in conjunction with your favourite property testing library.


Doc
===
* What it is and is for.
  * Mention: sync/async.
  * Mention: state/temporal-stateless.
  * Mention: embedable?
  * Mention: unit/functional/integration/property/fuzz
* Concepts + diagram (like Diode).
* Example applications.
* Comparison/conjunction with other libraries.
* What's available (modules).
* Sample output.


* How to use.
  * Setup. SBT & Exports.
  * Writing a test: Actions, checks, invariants.
  * Running a test.
* Operator/API reference.
* Example projects:
  * React TODO app. - domzipper, web, invariants, actions.
  * DB triggers.    - real external state, ref.
  * Mutable sample. - fuzz, invariants.


Each time an action is performed, R→O then (S,O)→S.

Show example code & report BEFORE Usage.
Results before how.


# Usage

#### Project Setup

1. Choose your modules. Add to SBT.

1. Create an `object` for your config and select the functionality you want.

  ```scala
  package your.package

  object Testate
    extends testate.Exports
    // with testate.TestStateCats
    // with testate.TestStateNyaya
    // with testate.TestStateScalaz
    // with testate.domzipper.sizzle.Exports
  {
    // Additional config here if desired.
  }
  ```

#### Writing Tests

Maybe point to tutorial on blog and just have a reference here.
```
import
dsl
actions
checks before|after|around
invariants
plan -> test -> report -> assert
```

1. Import your config.
  ```scala
  import your.package.Testate._
  ```
1. Create a DSL.
  ```scala
  val dsl = Dsl[Reference, Observation, StateExpectations]
  ```
1. Create checks (and focuses).
  ```scala
  // Focuses
  val moneyRemaining = dsl.focus("Money remaining").value(_.obs.money)
  val columns        = dsl.focus("Columns").collection(_.obs.selCols)

  // Checks
  val noMoneyRemaining = moneyRemaining.assert.equal(0)
  val itemCountMatches = dsl.focus("Item count").obsAndState(_.count, _.items.size).assert.equal
  ```
1. Create invariants.
  ```scala
  val invariants =
    columns.assert.distinct &
    columns.assert.contains("Name") &
    moneyRemaining.test("isn't negative")(_ >= 0)
  ```
1. Create actions.
  ```scala
  def addMoney(amount: Int) =
    dsl.action("Add $" + amount)(_.ref.addTransaction(amount))            // Action
      .updateState(s => s.copy(expectedMoney = s.expectedMoney + amount)) // Adjust expected state

  // Add before-, after-, and around-conditions using +>
  val logout = (
    loggedInUser.assert.not.equal(None)     // Pre-condition
    +> dsl.action("Logout")(_.ref.logout()) // Action
    +> loggedInUser.assert.equal(None)      // Post-condition #1
    +> pageId.assert.change                 // Post-condition #2
  )

  // Compose actions using >>
  val example =
    login >> addMoney(100).times(3) >> logout
  ```
1. Create test.
  ```scala
  val plan = Plan(actions, invariants)
  val test = plan test Observer(...)
  ```
1. Run test.
  ```scala
  val report = test.run(initialExpectedState, reference)

  // Asserts that test passed. Throws an exception if failed.
  // Prints a report on pass and/or failure, when & how configurable via implicit settings.
  report.assert()
  ```



# Modules
| Module              | Description | Platforms |
|---------------------|-------------|-----------|
| `core`              | The core module. | Scala + Scala.JS. |
| `supp-scalaz`       | Support for [Scalaz](https://github.com/scalaz/scalaz). | Scala + Scala.JS. |
| `supp-cats`         | Support for [Cats](https://github.com/typelevel/cats). | Scala + Scala.JS. |
| `supp-nyaya`        | Support for [Nyaya](https://github.com/japgolly/nyaya). | Scala + Scala.JS. |
| `dom-zipper`        | Utility for observing web DOM.<br>*(Requires [jQuery](https://jquery.com/) or [Sizzle](https://sizzlejs.com/).)* | Scala.JS only. |
| `dom-zipper-sizzle` | As above bundled with [Sizzle](https://sizzlejs.com/). | Scala.JS only. |
