

# How do I use this?

modules
deps
Create an object
types (Action etc)
types (FROSE)
create obs & dsl
create plan
run test


* How to use.
  * Setup. SBT & Exports.
  * Writing a test: Actions, checks, invariants.
  * Running a test.
* Operator/API reference.




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
