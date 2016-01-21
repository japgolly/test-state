package teststate

import scala.annotation.tailrec

sealed trait Result[+Err] {
  def failure: Option[Err]
}
object Result {
  case object Pass extends Result[Nothing] {
    override def failure = None
  }
  case object Skip extends Result[Nothing] {
    override def failure = None
  }
  case class Fail[+Err](error: Err) extends Result[Err] {
    override def failure = Some(error)
  }
}

case class Test0[-Ref, -Obs, State, +Err](action: Action[Ref, Obs, State, Err],
                                          invariants1: Invariants[Obs, State, Err] = Invariants.empty,
                                          invariants2: Checks[Obs, State, Err] = Checks.empty) {
  def observe[R <: Ref, O <: Obs](f: R => O) =
    Test(action, f, invariants1, invariants2)
}

case class Test[Ref, Obs, State, +Err](action: Action[Ref, Obs, State, Err],
                                       observe: Ref => Obs,
                                       invariants1: Invariants[Obs, State, Err] = Invariants.empty,
                                       invariants2: Checks[Obs, State, Err] = Checks.empty) {
  def run(initialState: State, ref: Ref): History[Err] =
    Runner.run(this)(initialState, ref)
}

object Runner {

  trait HalfCheck[O, S, Err] {
    type A
    val check: Check.Aux[O, S, Err, A]
    val before: A
  }
  def HalfCheck[O, S, Err, a](_check: Check.Aux[O, S, Err, a])(_before: a): HalfCheck[O, S, Err] =
    new HalfCheck[O, S, Err] {
      override type A     = a
      override val check  = _check
      override val before = _before
    }

  def run[Ref, Obs, State, Err](test: Test[Ref, Obs, State, Err])
//                               (observe: Ref => Obs)
                               (initialState: State, ref: Ref): History[Err] = {
import test.observe
    // TODO Catch all exceptions

    type A = Action[Ref, Obs, State, Err]
    type HS = History.Steps[Err]

    val invariantChecks = test.invariants1.toChecks & test.invariants2

    case class OMG(ros: ROS[Ref, Obs, State], history: HS)

    def start(a: A, ros: ROS[Ref, Obs, State], history: HS) =
      go(vector1(a), OMG(ros, history))

    @tailrec
    def go(queue: Vector[A], omg: OMG): OMG =
      if (queue.isEmpty)
        omg
      else {
        import omg._

        queue.head match {

          // ==============================================================================
          case Action.Single(nameFn, run, checks) =>
            val name = nameFn(ros.sos)

            def addHistory(result: Result[Err]) =
              omg.copy(history = history :+ History.Step(name, result))

            run(ros) match {
              case Some(act) =>

                halfChecks(checks & invariantChecks)(ros) match {
                  case Right(hcs) =>

                    act() match {
                      case Right(f) =>
                        val obs2 = observe(ref)
                        val state2 = f(obs2)

                        performChecks(hcs)(_.check name ros.sos, c => c.check.test(obs2, state2, c.before)) match {
                          case None =>
                            val h = History.Step(name, Result.Pass)
                            val omg2 = OMG(ROS(ref, obs2, state2), history :+ h)
                            go(queue.tail, omg2)
                          case Some(failedStep) =>
                            omg.copy(history = history :+ failedStep(name))
                        }


                      case Left(e) =>
                        addHistory(Result.Fail(e))
                    }

                  case Left(failedStep) =>
                    omg.copy(history = history :+ failedStep(name))
                }

              case None =>
                go(queue.tail, addHistory(Result.Skip))
            }

          // ==============================================================================
          case Action.Group(nameFn, children) =>
            val name = nameFn(ros.sos)
            val omg2 = start(children, ros, Vector.empty)
            val h2   = History(omg2.history)
            val omg3 = omg2.copy(history = omg.history :+ History.parent(name, h2))
            if (h2.failure.isDefined)
              omg3
            else
              go(queue.tail, omg3)

          // ==============================================================================
          case Action.Composite(actions) =>
            go(queue.tail ++ actions.toVector, omg)
        }
      }

    History {
      val initialObs = observe(ref)
      val ros = ROS(ref, initialObs, initialState)

      val firstSteps: HS = {
        val iv = test.invariants1.toVector
        if (iv.isEmpty)
          Vector.empty
        else {
          val children = iv
            .map { i =>
              val name = i.name(ros.sos)
              val result = i.test(initialObs, initialState).fold[Result[Err]](Result.Pass)(Result.Fail(_))
              History.Step(name, result)
            }
          vector1(History.parent("Initial checks.", History(children)))
        }
      }

      if (firstSteps.exists(_.failed))
        firstSteps
      else {
        val runResults = start(test.action, ros, Vector.empty).history
        val h = firstSteps ++ runResults
        if (runResults.exists(_.failed))
          h
        else
          h :+ History.Step("All pass.", Result.Pass)
      }
    }
  }

  private def halfChecks[O, S, E](checks: Checks[O, S, E])(ros: ROS[_, O, S])
  : Either[String => History.Step[E], Vector[HalfCheck[O, S, E]]] = {
    val r = Vector.newBuilder[HalfCheck[O, S, E]]
    val o = performChecks(checks.toVector)(
      _ name ros.sos,
      c0 => {
        val c = c0.aux
        c.before(ros.obs, ros.state) match {
          case Right(a) => r += HalfCheck(c)(a); None
          case Left(e) => Some(e)
        }
      }
    )
    o match {
      case None => Right(r.result())
      case Some(h) => Left(h)
    }
  }

  private def performChecks[A, E](as: Vector[A])(name: A => String, test: A => Option[E]): Option[String => History.Step[E]] = {
    var failures = List.empty[(Int, E)]

    for (i <- as.indices) {
      val a = as(i)
      test(a) match {
        case None    => ()
        case Some(e) => failures ::= ((i, e))
      }
    }

    if (failures.isEmpty)
      None
    else
      Some {
        val m = failures.toMap
        val history =
          as.indices.iterator.map { i =>
            val a = as(i)
            val n = name(a)
            val r = m.get(i).fold[Result[E]](Result.Pass)(Result.Fail(_))
            History.Step(n, r)
          }.toVector

        val firstFailure = failures.head._2
        History.Step(_, Result Fail firstFailure, History(history))
      }
  }

}