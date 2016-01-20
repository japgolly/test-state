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

  trait HalfCheck[O1, S1, O2, S2, Err] {
    type A
    val check: Check.Aux[O1, S1, O2, S2, Err, A]
    val before: A
  }
  def HalfCheck[O1, S1, O2, S2, Err, a](_check: Check.Aux[O1, S1, O2, S2, Err, a])(_before: a): HalfCheck[O1, S1, O2, S2, Err] =
    new HalfCheck[O1, S1, O2, S2, Err] {
      override type A     = a
      override val check  = _check
      override val before = _before
    }

  def run[Obs, State, Err](action: Action[Obs, State, Obs, State, Err],
                           invariants: Invariants[Obs, State, Err] = Invariants.empty,
                           invariants2: Checks[Obs, State, Obs, State, Err] = Checks.empty)
                          (initialState: State,
                           observe: () => Obs): History[Err, Unit] = {

    val invariantChecks = invariants.toChecks & invariants2

    case class OMG(obs: Obs, state: State, sos: Some[(Obs, State)], history: History.Steps[Err, Unit])

    def start(a: Action[Obs, State, Obs, State, Err], indent: Int, obs: Obs, state: State, sos: Some[(Obs, State)], history: History.Steps[Err, Unit]) =
      go(vector1(a), indent, OMG(obs, state, sos, history))

    @tailrec
    def go(queue: Vector[Action[Obs, State, Obs, State, Err]], indent: Int, omg: OMG): OMG =
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
            val name = nameFn(sos)

            def addHistory(result: Result[Err]) =
              omg.copy(history = addStep(name, result))

            def failedChecks(errors: TraversableOnce[Err]) =
            // TODO When up and running, put all checks in history, passes & failures
              addHistory(Result.Fail(errors.toList.head))

            run(obs, state) match {
              case Some(act) =>

                halfChecks(checks & invariantChecks)(obs, state) match {
                  case Right(hcs) =>

                    act() match {
                      case Right(f) =>
                        val obs2 = observe()
                        val state2 = f(obs2)

                        val afterFailures = hcs.iterator
                          .map(c => c.check.test(obs2, state2, c.before))
                          .filter(_.isDefined)
                          .map(_.get)

                        if (afterFailures.hasNext)
                          failedChecks(afterFailures)
                        else
                          go(queue.tail, indent, OMG(obs2, state2, Some((obs2, state2)), addStep(name, Result.Pass)))

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
            val name = nameFn(sos)
            val omg2 = start(children, indent + 1, obs, state, sos, Vector.empty)
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
      val sos = Some((initialObs, initialState))

      // TODO When up and running, put all checks in history, passes & failures
      val initialFailure =
        invariants.toVector.iterator
          .map(i => (i, i.test(initialObs, initialState)))
          .find(_._2.isDefined)
          .map { case (i, e) => History.Step(0, i.name(sos), Result.Fail(e.get), ()) }

      initialFailure match {
        case None    => start(action, 0, initialObs, initialState, sos, Vector.empty).history
        case Some(h) => vector1(h)
      }
    }
  }

  private def halfChecks[O1, S1, O2, S2, Err](checks: Checks[O1, S1, O2, S2, Err])(obs: O1, state: S1): Either[List[Err], List[HalfCheck[O1, S1, O2, S2, Err]]] = {
    var errors: List[Err] = Nil
    val b = List.newBuilder[HalfCheck[O1, S1, O2, S2, Err]]
    for (c0 <- checks.toVector) {
      val c = c0.aux
      c.before(obs, state) match {
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