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

case class Test0[Ref, Obs, State, +Err](action: Action[Ref, Obs, State, Err],
                                        invariants: Check[Obs, State, Err] = Check.empty) {
  def observe(f: Ref => Obs) =
    Test(action, f, invariants)
}

case class Test[Ref, Obs, State, +Err](action: Action[Ref, Obs, State, Err],
                                       observe: Ref => Obs,
                                       invariants: Check[Obs, State, Err] = Check.empty) {
  def run(initialState: State, ref: Ref): History[Err] =
    Runner.run(this)(initialState, ref)

  // TODO add invariants
}

object Runner {

  trait HalfCheck[O, S, Err] {
    type A
    val check: Check.Around.SingleA[O, S, Err, A]
    val before: A
  }
  def HalfCheck[O, S, Err, a](_check: Check.Around.SingleA[O, S, Err, a])(_before: a): HalfCheck[O, S, Err] =
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
    type ROS = teststate.ROS[Ref, Obs, State]

    val invariantsAround = test.invariants.around
    val invariantsPoints = test.invariants.point.singles

    case class OMG(ros: ROS, history: HS) {
      def addHistory(name: String, result: Result[Err]): OMG =
        addHistory(name, result, History.empty)

      def addHistory(name: String, result: Result[Err], children: History[Err]): OMG =
        addHistory(History.Step(name, result, children))

      def addHistory(step: History.Step[Err]): OMG =
        copy(history = history :+ step)
    }

    // TODO Either[OMG, OMG] <-- gross
    def checkAround[A](name: String, check: Check.Around[Obs, State, Err], omg: OMG)
                      (prepare: ROS => Option[A])
                      (run: A => Either[String => History.Step[Err], OMG]): Either[OMG, OMG] =
      prepare(omg.ros) match {
        case Some(a) =>
          halfChecks(check)(omg.ros)
            .fmap(hcs =>
              run(a)
                .check(omg2 => performChecks(hcs)(_.check name omg.ros.sos, c => c.check.test(omg2.ros.obs, omg2.ros.state, c.before)))
                .check(omg2 => performChecks(invariantsPoints)(_ name omg.ros.sos, c => c.test(omg2.ros.obs, omg2.ros.state)))
            )
            .leftMap(f => omg.addHistory(f(name)))
        case None =>
          Right(omg.addHistory(name, Result.Skip))
      }

    def start(a: A, ros: ROS) =
      go(vector1(a), OMG(ros, Vector.empty))

    @tailrec
    def go(queue: Vector[A], omg: OMG): OMG =
      if (queue.isEmpty)
        omg
      else {
        import omg.ros

        queue.head match {

          // ==============================================================================
          case Action.Single(nameFn, run, check) =>
            val name = nameFn(ros.sos)
            checkAround(name, check & invariantsAround, omg)(run)(act =>
              act() match {
                case Right(f) =>
                  val obs2 = observe(ref)
                  val state2 = f(obs2)
                  Right(OMG(ROS(ref, obs2, state2), omg.history :+ History.Step(name, Result.Pass)))
                case Left(e) => Left(History.Step(_, Result.Fail(e)))
              }
            ) match {
              case Right(omg2) => go(queue.tail, omg2)
              case Left(omg2) => omg2
            }

          // ==============================================================================
          case Action.Group(nameFn, actionFn, check) =>
            val name = nameFn(ros.sos)
            checkAround(name, check & invariantsAround, omg)(actionFn)(children => {
              val childrenResults = start(children, ros)
              val childrenHistory = History(childrenResults.history)
              if (childrenHistory.failed)
                Left(History.parent(_, childrenHistory))
              else
                Right {
                  val groupStep = History.parent(name, childrenHistory)
                  OMG(childrenResults.ros, omg.history :+ groupStep)
                }
            }) match {
              case Right(omg2) => go(queue.tail, omg2)
              case Left(omg2) => omg2
            }

          // ==============================================================================
          case Action.Composite(actions) =>
            go(queue.tail ++ actions.toVector, omg)
        }
      }

    History {
      val initialObs = observe(ref)
      val ros = ROS(ref, initialObs, initialState)

      val firstSteps: HS =
        if (invariantsPoints.isEmpty)
          Vector.empty
        else {
          val children = invariantsPoints.map { i =>
            val name = i.name(ros.sos)
            val result = i.test(initialObs, initialState).fold[Result[Err]](Result.Pass)(Result.Fail(_))
            History.Step(name, result)
          }
          vector1(History.parent("Initial checks.", History(children)))
        }

      if (firstSteps.exists(_.failed))
        firstSteps
      else {
        val runResults = start(test.action, ros).history
        val h = firstSteps ++ runResults
        if (runResults.exists(_.failed))
          h
        else
          h :+ History.Step("All pass.", Result.Pass)
      }
    }
  }

  private def halfChecks[O, S, E](checks: Check.Around[O, S, E])(ros: ROS[_, O, S])
  : Either[String => History.Step[E], Vector[HalfCheck[O, S, E]]] = {
    val r = Vector.newBuilder[HalfCheck[O, S, E]]
    val o = performChecks(checks.singles)(
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