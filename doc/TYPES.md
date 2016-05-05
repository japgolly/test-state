# Abstract Types

Most built-in structures have abstract types for you to plug in your own types.

Their names are consistent and match the following:

| Type   | Desc |
|--------|------|
| `F[_]` | The context in which test execution is performed.<br>By default, this is the built-in type alias `Id`; for asynchronous tests, `scala.concurrent.Future`.
| `R`    | Reference. *(optional)*<br>Some kind of arbitrary (non-strict) data that is available to all parts of the test. It can be used to perform actions or make observations. |
| `O`    | Observation. *(optional)*<br>An immutable snapshot of the stateful world that captures anything relevant to the test. |
| `S`    | State for testing. *(optional)*<br>Data created and updated by the test. Used to keep track of expectations about the test subject.<br>**This is not** the mutable/stateful test subject; this is **immutable**, and updated by test `Actions`. |
| `E`    | Error.<br>Representation of assertion failure or exceptions. By default, this is `String`. |


##### Example

As an example, if you were testing a database you might have the following:
* `R` = `java.sql.Connection` - A connection to the subject database.
* `O` = `Vector[String]` - All user names in the DB. The result of `select name from user`.
* `S` = `Int` - The expected number of user names.
* An action called `addUser` that issues a `insert ? into user` and sets `S := S + 1`.
* An invariant that specifies that the number of real users `(_: O).length` matches `S`.



# Primary Types

| Type | Desc |
|------|------|
| `Actions[F[_], R, O, S, E]` | Zero or more actions that affect the impure world and possibly update test state expectations. Executed one at a time. |
| `Invariants[-O, -S, E]` | Zero or more properties that should always be true. |
| `Points[-O, -S, E]` | Zero or more assertions at a single *Point* in time.<br>Eg. *x is 7*. |
| `Arounds[-O, -S, E]` | Zero or more assertions *Around* a period of time.<br>Eg. *x was 7*, *x increased by 2*. |
| `Dsl[F[_], R, O, S, E]` | Provides you with a library of functions. See [DSL.md](DSL.md). |
| `ROS[+R, +O, +S]` | Same as a `(R,O,S)` tuple except instead of `_1`, `_2`, `_3` it has `ref`, `obs`, `state`. |
| `OS[+O, +S]` | Same as a `(O,S)` tuple except instead of `_1`, `_2` it has `obs`, `state`. |
| `Name` | A lazy `String` that specifies the name of an action or assertion. |
| `NameFn[-A]` | A function from `Option[A]` to a `Name`.<br>Think of it as a `Name` that becomes more detailed when an `A` becomes available.<br>Example: It could return `"Check counter"` on `None`, and `"Check counter is 7"` on `Some(7)`. |
| `Report[+E]` | Result of test execution. You'll often just call `.assert()` on this to assert that the test passed. |


Using the above, you create the content of your tests.
In order to run tests, you combine data as follows:

![Runner hierarchy](https://rawgit.com/japgolly/test-state/master/doc/runner.gv.svg)

Example:
```scala
class Ref
class Obs(ref: Ref)

val dsl = Dsl[Ref, Obs, Int]

val action = dsl.emptyAction        // Actions   [Id, Ref, Obs, Int, String]
val invariants = dsl.emptyInvariant // Invariants         [Obs, Int, String]

Plan(action, invariants)            // Plan                [Id, Ref, Obs, Int, String]
  .withInitialState(123)            // PlanWithInitialState[Id, Ref, Obs, Int, String]
  .test(Observer(new Obs(_)))       // TestWithInitialState[Id, Ref, Obs, Int, String]
  .run(new Ref)                     // Id[Report[String]] = Report[String]
```

# Secondary Types

| Type | Desc |
|------|------|
| `Display[A]` | Typeclass used by `Dsl` assertions to turn expectation and actual values into `String`s for display. (For humans, unlike common `Show[A]`.) |
| `Equal[A]` | Typeclass used by `Dsl` assertions for equality checking. Implicit conversions exist from: <ul><li>`japgolly.univeq.UnivEq[A]`.</li><li>`cats.Eq[A]` if using the Cats module.</li><li>`scalaz.Equal` if using the Scalaz module.</li></ul> |
| `ExecutionModel[F[_]]` | Typeclass required to run tests in custom contexts. |
| `Observer[-R, +O, +E]` | Creates observations. As indicate by type variance, ref comes in, observation or error come out. |
| `Or[+A,+B]` | Disjunction. Internal replacement for stdlib's atrocious `Either`. Implicit conversions exist from: <ul><li>`scala.util.Either[A, B]`.</li><li>`cats.data.Xor[A,B]` if using the Cats module.</li><li>`scalaz.\/[A,B]` if using the Scalaz module.</li></ul> |
| `Recover[+E]` | Turns a `Throwable` into an `E`. |
| `Report.AssertionSettings` | Typeclass that specifies how to display pass/fail results on a `Report`. |
