# Composability

- [Symbolic Operators](#symbolic-operators)
- [Transformers](#transformers)
- [Plans and Tests](#plans-and-tests)

# Symbolic Operators

There are 3 symbolic operators in this library.

| Operator | Desc |
|----------|------|
| `&`      | Composes assertions (`Points`, `Arounds`, `Invariants`). |
| `>>`     | Sequences `Actions`. |
| `+>`     | Adds assertions to `Actions`. |

| Expression | Result | Desc |
|------------|--------|------|
| `Points & Points` | `Points` | Composes Point-assertions. |
| `Arounds & Arounds` | `Arounds` | Composes Around-assertions. |
| `Invariants & {any assertion type}`<br>`{any assertion type} & Invariants` | `Invariants` | Composes assertions into invariants. |
| `Actions >> Actions` | `Actions` | Composes actions so that they run sequentially. |
| `Points +> Actions` | `Actions` | Adds pre-conditions to actions. |
| `Actions +> Points`<br>`Actions +> Arounds` | `Actions` | Adds post-conditions to actions. |

In all cases above, you can see that the `>` indicates the order of execution, left-to-right.

Operators can also be written backwards. These are equivalent:

| Forwards | Backwards |
|----------|-----------|
| `a >> b` | `b << a`  |
| `a +> b` | `b <+ a`  |

As above, the `>` and `<` chars indicate execution flow.

In this example, each term will be executed in the order it appears:
```scala
preCond1
+> action1
+> postCond1a
+> postCond1b
>> action2
+> postCond2
```


## Transformers

A `Transformer` is a means of changing any or all of the types in a test constituent.
This allows different test (or test components) to be transformed to have matching types
where they can be composed normally. This allows entire tests to be embedded in others.

To create a transformer, start with `.transformer` on your `Dsl`,
then call as many of its mapping methods as needed to specify how to change types.

Once done, you can either use the transformer directly to transform test constituents,
or you can make it `implicit` and call `.lift` on test constituents such as `Actions`, `Invariants`, `Plan`s, etc.


## Plans and Tests

Create a `Plan` from `Actions` and/or `Invariants` by using one of:
* `Plan.apply`
* `Plan.actions`
* `Plan.invariants`

Add/modify `Actions` and/or `Invariants` to an existing `Plan` by using one of:
* `plan.addInvariants`
* `plan.modActions`
* `plan.modInvariants`

Turn a `Plan` into an `Action` using `.asAction`.

Extract `Actions` and `Invariants` from `Plan` using `.actions`, `.invariants`.

Turn a `Plan` into a `Test` (and vice-versa similarly) like this:

![Runner hierarchy](https://cdn.rawgit.com/japgolly/test-state/master/doc/runner.gv.svg)
