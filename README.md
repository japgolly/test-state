# Scala Test-State

Test stateful stuff statelessly, and reasonably.

# Contents

- [What is this?](#what-is-this)
- [How does this work?](#how-does-this-work)
- How do I use this?
  - [Usage](doc/USAGE.md).
  - [Types](doc/TYPES.md).
  - [DSL](doc/DSL.md).
- Modules
- Examples
- Changelog


# What is this?

Firstly, a quick mention of what this *is not*:

1. This is not a test *framework*.
   <br>Use it conjunction with ScalaTest, Specs2, μTest, etc.

1. This is not a property testing library.
   <br>Use it conjunction with ScalaCheck, Nyaya, ScalaProps, etc.

Ok, so what *is* this?
This is a library that:

1. Lets you write pure, immutable, referentially-transparent tests
   that verify stateful, effectful code or data.

1. Encourages composability of test concepts such as invariants/properties, pre/post conditions,
   dynamic actions/assertions, and more.

1. Makes test failure and inspection easy to comprehend.

### Uses

* Unit-test a webapp with Scala.JS.
* Test DB triggers, or a DB migration.
* Integration-test.
* Random-test (fuzz-test) like Android's `monkeyrunner` or ScalaCheck's `Command` API.
* UAT automation.


### Features

* Compiled for Scala & Scala.JS.
* Can run synchronously, asynchronously (`Future`) or in your own context-type (eg `Task`). Is stack-safe.
* Everything is immutable and composable.
* Everything can be transformed into (reused in) different contexts.
* Combines property and imperative testing.
* Actions and assertions can be non-deterministic and/or dependent on runtime state.
* Tries to be as transparent and informative as possible about test execution.
* Optionally configurable error type. Use a custom ADT to precisely maintain all forms of failure and error in your domain.
* Includes a utility called `DomZipper` which greatly simplifies the task of HTML/SVG observation.
* Extension modules for various 3rd-party libraries. (Scalaz, Cats, more.)


# How does this work?

The key is to take **observations** of anything relevant in the stateful test subject.
Observations are like immutable snapshots.
They capture what the state was at a particular point in time.
Once an observation is captured, assertions are performed on it.

Optionally, you can specify some kind of test-only state that you modify as you test
and use to ensure the real-world observations are what you expect.
<br>For example, if you're testing a bank account app, you could maintain your own expected balance such that
when you instruct the app to make a deposit, you add the same amount to your state.
You could then add an invariant that whenever the balance is shown in the app, it matches the expected state balance.

This is a (simplified) model of how tests are executed:

![concept](doc/concept.uml.png)

Here is what the above looks like in action:

![example output](example-react/output-failure.png)


# Modules

| Module              | Description | Platforms |
|---------------------|-------------|-----------|
| `core`              | The core module. | Scala + Scala.JS. |
| `dom-zipper`        | Utility for observing web DOM.<br>*(Requires [jQuery](https://jquery.com/) or [Sizzle](https://sizzlejs.com/).)* | Scala.JS only. |
| `dom-zipper-sizzle` | As above bundled with [Sizzle](https://sizzlejs.com/). | Scala.JS only. |
| `cats`         | Support for [Cats](https://github.com/typelevel/cats). | Scala + Scala.JS. |
| `nyaya`        | Support for [Nyaya](https://github.com/japgolly/nyaya). | Scala + Scala.JS. |
| `scalajs-react` | Support for [scalajs-react](https://github.com/japgolly/scalajs-react). | Scala.JS only. |
| `scalaz`       | Support for [Scalaz](https://github.com/scalaz/scalaz). | Scala + Scala.JS. |


# Examples

* React TODO app. - domzipper, web, invariants, actions.
* DB triggers.    - real external state, ref.
* Mutable sample. - fuzz, invariants.
