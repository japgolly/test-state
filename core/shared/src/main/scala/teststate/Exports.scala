package teststate

import teststate.data.Or

trait Exports
  extends core.CoreExports
     with core.CoreExports2
     with core.Transformer.ToOps {

  type Dsl[F[_], R, O, S, E] = teststate.dsl.Dsl[F, R, O, S, E]
  val Dsl = teststate.dsl.Dsl

  type Recover[+E] = teststate.typeclass.Recover[E]
  val Recover = teststate.typeclass.Recover

  type ExecutionModel[M[_]] = teststate.typeclass.ExecutionModel[M]
  val ExecutionModel = teststate.typeclass.ExecutionModel

  type Equal[E] = teststate.typeclass.Equal[E]
  val Equal = teststate.typeclass.Equal

  type Show[E] = teststate.typeclass.Show[E]
  val Show = teststate.typeclass.Show

  type ShowError[E] = teststate.typeclass.ShowError[E]
  val ShowError = teststate.typeclass.ShowError

  type Name = teststate.data.Name
  val Name = teststate.data.Name

  /*
  type Or[+A, +B] = teststate.data.Or[A, B]
  val Or = teststate.data.Or

  type Left[+A, +B] = teststate.data.Left[A, B]
  val Left = teststate.data.Left

  type Right[+A, +B] = teststate.data.Right[A, B]
  val Right = teststate.data.Right
  */

  type TestContent[F[_], R, O, S, E] = teststate.run.TestContent[F, R, O, S, E]

  type Test[F[_], R, O, S, E] = teststate.run.Test[F, R, O, S, E]
  val Test = teststate.run.Test

  type History[+E] = teststate.run.History[E]
  val History = teststate.run.History

  type Result[+E] = teststate.data.Result[E]
  val Result = teststate.data.Result

  implicit def testStateOrFromScalaEither[A, B](e: A Either B): A Or B =
    Or fromScalaEither e
}

object Exports extends Exports
