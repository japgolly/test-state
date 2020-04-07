package teststate.run

import acyclic.file
import scala.collection.compat._
import teststate.data._
import teststate.typeclass.{ErrorHandler, DisplayError}
import History.{Step, Steps}
import Result.{Fail, Skip, Pass}

final class History[+E](val steps: Steps[E], val result: Result[E]) {
  override def toString = s"History($steps, $result)"

  def failure = result.failure
  def failed = failure.isDefined
  def isEmpty = steps.isEmpty
  @inline def nonEmpty = !isEmpty

  def :+[e >: E](s: Step[e]) = this ++ vector1(s)
  def ++[e >: E](s: Steps[e]): History[e] = {
    val result2 = s.foldLeft[Result[e]](result)(_ + _.result)
    new History[e](steps ++ s, result2)
  }
  def ++[e >: E](s: History[e]): History[e] =
    new History[e](steps ++ s.steps, result + s.result)

  def resultStep: Option[Step[E]] =
    steps.find(_.result eq result)

  def rootFailurePath: Vector[Step[E]] =
    if (failed)
      resultStep match {
        case Some(s) => s +: s.children.rootFailurePath
        case None => Vector.empty
      }
    else
      Vector.empty

  //def +:[e >: E](s: Step[e]) = new History[e](s +: steps, result)
  //def ++[e >: E](s: Steps[e]) = new History[e](steps ++ s, result)

  def unlessFailed[e >: E](f: History[E] => History[e]): History[e] =
    if (failed)
      this
    else
      f(this)
}


object History {
  val empty = History(Vector.empty, Result.empty)

  type Steps[+Err] = Vector[Step[Err]]

  case class Step[+E](name: Name, result: Result[E], children: History[E] = empty) {
    def failure = result.failure
    def failed = failure.isDefined
  }

  object Step {
    def skip[E](nameFn: NameFn[Nothing])(implicit attempt: ErrorHandler[E]) =
      Step(attempt.name(nameFn, None), Skip)
  }

  def parent[E](name: Name, children: History[E]): Step[E] =
    Step(name, children.result, children)

  def maybeParent[E](name: Name, children: History[E]): History[E] =
    if (children.isEmpty)
      History.empty
    else
      History(parent(name, children))

  def determineResult[E](steps: Steps[E]): Result[E] =
    steps.foldLeft(Result.empty[E])(_ + _.result)

  def apply[E](step: Step[E]): History[E] =
    new History(vector1(step), step.result)

  def apply[E](steps: Steps[E]): History[E] =
    new History(steps, determineResult(steps))

  def apply[E](steps: Steps[E], result: Result[E]): History[E] =
    new History(steps, result)

  def newBuilder[E](stats: Stats.Mutable) = new Builder[E](stats)
  final class Builder[E](stats: Stats.Mutable) {
    type FE = Failure[E]
    private val b = Vector.newBuilder[Step[FE]]
    private var r = Result.empty[FE]

    def +=(s: Step[FE]): Unit = {
      b += s
      r += s.result
      if (s.result != Skip)
        stats.checks += 1
    }

    def ++=(h: History[FE]): Unit =
      h.steps foreach (this += _)

    def add1[A, B](a: A)(nameFn: A => NameFn[B])(nameInput: Some[B], test: A => Tri[FE, Any])(implicit attempt: ErrorHandler[E]): Unit = {
      val n = attempt.name(nameFn(a), nameInput)
      val r = attempt.recover(test(a).toResult, Fail(_))
      this += Step(n, r)
    }

    def addNE(ne: NamedError[FE]): Unit =
      this += Step(ne.name, Fail(ne.error))

    def addEach[A, B](as: IterableOnce[A])(nameFn: A => NameFn[B])(nameInput: Some[B], test: A => Tri[FE, Any])(implicit attempt: ErrorHandler[E]): Unit =
      for (a <- as.iterator)
        add1(a)(nameFn)(nameInput, test)(attempt)

    def addEachNE[A, B](as: IterableOnce[NamedError[FE] Or A])(nameFn: A => NameFn[B])(nameInput: Some[B], test: A => Tri[FE, Any])(implicit attempt: ErrorHandler[E]): Unit =
      as.iterator.foreach {
        case Right(a) => add1(a)(nameFn)(nameInput, test)(attempt)
        case Left(ne) => addNE(ne)
      }

    def failed(): Boolean =
      r.failure.isDefined

    def result(): Result[FE] =
      r

    def steps(): Steps[FE] =
      b.result()

    def history(): History[FE] =
      History(steps(), result())

    def group(name: Name): History[FE] =
      History.maybeParent(name, history())
  }
}
