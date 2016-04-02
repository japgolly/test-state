package testate

import acyclic.file
import testate.data.Or

trait Exports
  extends core.CoreExports
     with core.CoreExports2
     with core.CoreExports3
     with typeclass.Empty.Ops
     with typeclass.Equal.Implicits
     with run.RunImplicits {

  // TODO Decide on project name. Prefix ALL implicits values for Equal etc.

  type Dsl[F[_], R, O, S, E] = testate.dsl.Dsl[F, R, O, S, E]
  val Dsl = testate.dsl.Dsl

  type Recover[+E] = testate.typeclass.Recover[E]
  val Recover = testate.typeclass.Recover

  type ExecutionModel[M[_]] = testate.typeclass.ExecutionModel[M]
  val ExecutionModel = testate.typeclass.ExecutionModel

  type Empty[+E] = testate.typeclass.Empty[E]
  val Empty = testate.typeclass.Empty

  type Equal[E] = testate.typeclass.Equal[E]
  val Equal = testate.typeclass.Equal

  type Display[E] = testate.typeclass.Display[E]
  val Display = testate.typeclass.Display

  type DisplayError[E] = testate.typeclass.DisplayError[E]
  val DisplayError = testate.typeclass.DisplayError

  type Name = testate.data.Name
  val Name = testate.data.Name

  type NameFn[-A] = testate.data.NameFn[A]
  val NameFn = testate.data.NameFn

  /*
  type Or[+A, +B] = testate.data.Or[A, B]
  val Or = testate.data.Or

  type Left[+A, +B] = testate.data.Left[A, B]
  val Left = testate.data.Left

  type Right[+A, +B] = testate.data.Right[A, B]
  val Right = testate.data.Right
  */

  type Plan[F[_], R, O, S, E] = testate.run.Plan[F, R, O, S, E]
  val Plan = testate.run.Plan

  type PlanWithInitialState[F[_], R, O, S, E] = testate.run.PlanWithInitialState[F, R, O, S, E]
  val PlanWithInitialState = testate.run.PlanWithInitialState

  type Test[F[_], R, O, S, E] = testate.run.Test[F, R, O, S, E]
  val Test = testate.run.Test

  type TestWithInitialState[F[_], R, O, S, E] = testate.run.TestWithInitialState[F, R, O, S, E]
  val TestWithInitialState = testate.run.TestWithInitialState

  type Observer[-R, +O, +E] = testate.run.Observer[R, O, E]
  val Observer = testate.run.Observer

  type Result[+E] = testate.data.Result[E]
  val Result = testate.data.Result

  type Report[+E] = testate.run.Report[E]
  val Report = testate.run.Report

  implicit def testStateOrFromScalaEither[A, B](e: A Either B): A Or B =
    Or fromScalaEither e

  // TODO Separate Defaults ↓ from Exports ↑

  implicit def assertionSettings: Report.AssertionSettings =
    Report.AssertionSettings.coloured
}

object Exports extends Exports
