package teststate

import acyclic.file
import teststate.data.Or

trait Exports
  extends core.CoreExports
     with core.CoreExports2
     with core.CoreExports3
     with typeclass.Empty.Ops
     with typeclass.Equal.Implicits
     with run.RunImplicits {

  // TODO Decide on project name. Prefix ALL implicits values for Equal etc.

  type Dsl[F[_], R, O, S, E] = teststate.dsl.Dsl[F, R, O, S, E]
  val Dsl = teststate.dsl.Dsl

  type Recover[+E] = teststate.typeclass.Recover[E]
  val Recover = teststate.typeclass.Recover

  type ExecutionModel[M[_]] = teststate.typeclass.ExecutionModel[M]
  val ExecutionModel = teststate.typeclass.ExecutionModel

  type Empty[+E] = teststate.typeclass.Empty[E]
  val Empty = teststate.typeclass.Empty

  type Equal[E] = teststate.typeclass.Equal[E]
  val Equal = teststate.typeclass.Equal

  type Display[E] = teststate.typeclass.Display[E]
  val Display = teststate.typeclass.Display

  type DisplayError[E] = teststate.typeclass.DisplayError[E]
  val DisplayError = teststate.typeclass.DisplayError

  type Name = teststate.data.Name
  val Name = teststate.data.Name

  type NameFn[-A] = teststate.data.NameFn[A]
  val NameFn = teststate.data.NameFn

  /*
  type Or[+A, +B] = teststate.data.Or[A, B]
  val Or = teststate.data.Or

  type Left[+A, +B] = teststate.data.Left[A, B]
  val Left = teststate.data.Left

  type Right[+A, +B] = teststate.data.Right[A, B]
  val Right = teststate.data.Right
  */

  type Plan[F[_], R, O, S, E] = teststate.run.Plan[F, R, O, S, E]
  val Plan = teststate.run.Plan

  type PlanWithInitialState[F[_], R, O, S, E] = teststate.run.PlanWithInitialState[F, R, O, S, E]
  val PlanWithInitialState = teststate.run.PlanWithInitialState

  type Test[F[_], R, O, S, E] = teststate.run.Test[F, R, O, S, E]
  val Test = teststate.run.Test

  type TestWithInitialState[F[_], R, O, S, E] = teststate.run.TestWithInitialState[F, R, O, S, E]
  val TestWithInitialState = teststate.run.TestWithInitialState

  type Observer[-R, +O, +E] = teststate.run.Observer[R, O, E]
  val Observer = teststate.run.Observer

  type Result[+E] = teststate.data.Result[E]
  val Result = teststate.data.Result

  type Report[+E] = teststate.run.Report[E]
  val Report = teststate.run.Report

  implicit def testStateOrFromScalaEither[A, B](e: A Either B): A Or B =
    Or fromScalaEither e

  // TODO Separate Defaults ↓ from Exports ↑

  implicit def assertionSettings: Report.AssertionSettings =
    Report.AssertionSettings.coloured
}

object Exports extends Exports
