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

object Runner {

  trait HalfCheck[S1, O1, S2, O2, Err] {
    type A
    val check: Check.Aux[S1, O1, S2, O2, Err, A]
    val before: A
  }
  def HalfCheck[S1, O1, S2, O2, Err, a](_check: Check.Aux[S1, O1, S2, O2, Err, a])(_before: a): HalfCheck[S1, O1, S2, O2, Err] =
    new HalfCheck[S1, O1, S2, O2, Err] {
      override type A     = a
      override val check  = _check
      override val before = _before
    }

  def run[State, Obs, Err](action: Action[State, Obs, State, Obs, Err],
                           invariants: Invariants[State, Obs, Err] = Invariants.empty,
                           invariants2: Checks[State, Obs, State, Obs, Err] = Checks.empty)
                          (initialState: State,
                           observe: () => Obs): History[Err, Unit] = {

    val invariantChecks = invariants.toChecks & invariants2

    case class OMG(state: State, obs: Obs, sso: Some[(State, Obs)], history: History.Steps[Err, Unit])

    def start(a: Action[State, Obs, State, Obs, Err], indent: Int, state: State, obs: Obs, sso: Some[(State, Obs)], history: History.Steps[Err, Unit]) =
      go(vector1(a), indent, OMG(state, obs, sso, history))

    @tailrec
    def go(queue: Vector[Action[State, Obs, State, Obs, Err]], indent: Int, omg: OMG): OMG =
      if (queue.isEmpty)
        omg
      else {
        import omg._

        def step(name: String, result: Result[Err]) =
          History.Step(indent, name, result, ())

        def addStep(name: String, result: Result[Err]) =
          history :+ step(name, result)

        queue.head match {

          // ==============================================================================
          case Action.Single(nameFn, run, checks) =>
            val name = nameFn(sso)

            def addHistory(result: Result[Err]) =
              omg.copy(history = addStep(name, result))

            def failedChecks(errors: TraversableOnce[Err]) =
            // TODO When up and running, put all checks in history, passes & failures
              addHistory(Result.Fail(errors.toList.head))

            run(state, obs) match {
              case Some(act) =>

                halfChecks(checks & invariantChecks)(state, obs) match {
                  case Right(hcs) =>

                    act() match {
                      case Right(f) =>
                        val obs2 = observe()
                        val state2 = f(obs2)

                        val afterFailures = hcs.iterator
                          .map(c => c.check.test(state2, obs2, c.before))
                          .filter(_.isDefined)
                          .map(_.get)

                        if (afterFailures.hasNext)
                          failedChecks(afterFailures)
                        else
                          go(queue.tail, indent, OMG(state2, obs2, Some((state2, obs2)), addStep(name, Result.Pass)))

                      case Left(e) =>
                        addHistory(Result.Fail(e))
                    }

                  case Left(errors) =>
                    failedChecks(errors)
                }

              case None =>
                go(queue.tail, indent, addHistory(Result.Skip))
            }

          // ==============================================================================
          case Action.Group(nameFn, children) =>
            val name = nameFn(sso)
            val omg2 = start(children, indent + 1, state, obs, sso, Vector.empty)
            var failed = false
            val result =
              if (omg2.history.isEmpty)
                Result.Pass
              else {
                var skipSeen = false
                var lastError: Option[Result.Fail[Err]] = None
                omg2.history foreach (_.result match {
                  case Result.Pass => ()
                  case Result.Skip => skipSeen = true
                  case e: Result.Fail[Err] => lastError = Some(e); failed = true
                })
                lastError.getOrElse(if (skipSeen) Result.Skip else Result.Pass)
              }
            val omg3 = omg2.copy(history = addStep(name, result) ++ omg2.history)

            if (failed)
              omg3
            else
              go(queue.tail, indent, omg3)

          // ==============================================================================
          case Action.Composite(actions) =>
            go(queue.tail ++ actions.toVector, indent, omg)
        }
      }

    History {
      val initialObs = observe()
      val sso = Some((initialState, initialObs))

      // TODO When up and running, put all checks in history, passes & failures
      val initialFailure =
        invariants.toVector.iterator
          .map(i => (i, i.test(initialState, initialObs)))
          .find(_._2.isDefined)
          .map { case (i, e) => History.Step(0, i.name(sso), Result.Fail(e.get), ()) }

      initialFailure match {
        case None    => start(action, 0, initialState, initialObs, sso, Vector.empty).history
        case Some(h) => vector1(h)
      }
    }
  }

  private def halfChecks[S1, O1, S2, O2, Err](checks: Checks[S1, O1, S2, O2, Err])(state: S1, obs: O1): Either[List[Err], List[HalfCheck[S1, O1, S2, O2, Err]]] = {
    var errors: List[Err] = Nil
    val b = List.newBuilder[HalfCheck[S1, O1, S2, O2, Err]]
    for (c0 <- checks.toVector) {
      val c = c0.aux
      c.before(state, obs) match {
        case Right(a) => b += HalfCheck(c)(a)
        case Left(e) => errors ::= e
      }
    }
    if (errors.isEmpty)
      Right(b.result())
    else
      Left(errors)
  }

}