package teststate

import teststate.data.Or

trait Exports
  extends core.CoreExports
     with core.CoreExports2
     with core.CoreExports3
     with typeclass.Empty.Ops
     with typeclass.Equal.Implicits
     with run.RunImplicits
     with util.Exports {

  type Display[E] = teststate.typeclass.Display[E]
  val Display = teststate.typeclass.Display

  type DisplayError[E] = teststate.typeclass.DisplayError[E]
  val DisplayError = teststate.typeclass.DisplayError

  type Dsl[F[_], R, O, S, E] = teststate.dsl.Dsl[F, R, O, S, E]
  val Dsl = teststate.dsl.Dsl

  type Empty[+E] = teststate.typeclass.Empty[E]
  val Empty = teststate.typeclass.Empty

  type Equal[E] = teststate.typeclass.Equal[E]
  val Equal = teststate.typeclass.Equal

  type ExecutionModel[M[_]] = teststate.typeclass.ExecutionModel[M]
  val ExecutionModel = teststate.typeclass.ExecutionModel

  type Name = teststate.data.Name
  val Name = teststate.data.Name

  type NameFn[-A] = teststate.data.NameFn[A]
  val NameFn = teststate.data.NameFn

  type Observer[-R, +O, +E] = teststate.run.Observer[R, O, E]
  val Observer = teststate.run.Observer

  type Plan[F[_], R, O, S, E] = teststate.run.Plan[F, R, O, S, E]
  val Plan = teststate.run.Plan

  type PlanWithInitialState[F[_], R, O, S, E] = teststate.run.PlanWithInitialState[F, R, O, S, E]
  val PlanWithInitialState = teststate.run.PlanWithInitialState

  type ErrorHandler[+E] = teststate.typeclass.ErrorHandler[E]
  val ErrorHandler = teststate.typeclass.ErrorHandler

  type DisplayFailure[-A, +E] = teststate.dsl.DisplayFailure[A, E]
  val DisplayFailure = teststate.dsl.DisplayFailure

  type Report[+E] = teststate.run.Report[E]
  val Report = teststate.run.Report

  type Result[+E] = teststate.data.Result[E]
  val Result = teststate.data.Result

  val Retry = teststate.run.Retry

  type Test[F[_], R, O, S, E] = teststate.run.Test[F, R, O, S, E]
  val Test = teststate.run.Test

  type TestWithInitialState[F[_], R, O, S, E] = teststate.run.TestWithInitialState[F, R, O, S, E]
  val TestWithInitialState = teststate.run.TestWithInitialState

  implicit def testStateOrFromScalaEither[A, B](e: A Either B): A Or B =
    Or fromScalaEither e

  implicit def testStateAssertionSettings: Report.AssertionSettings =
    Report.AssertionSettings.default

  implicit def testStateErrorHandler: ErrorHandler[String] =
    ErrorHandler.byToString
}

object Exports extends Exports
