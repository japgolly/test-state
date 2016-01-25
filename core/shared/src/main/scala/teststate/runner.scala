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
  def passOrFail[E](o: Option[E]): Result[E] =
    o.fold[Result[E]](Pass)(Fail(_))
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
    val check: Check.Around.DunnoA[O, S, Err, A]
    val before: A
  }
  def HalfCheck[O, S, Err, a](_check: Check.Around.DunnoA[O, S, Err, a])(_before: a): HalfCheck[O, S, Err] =
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

    case class OMG(ros: ROS, history: History[Err]) {
      def failure = history.failure
      def failed = history.failed

      def :+(s: History.Step[Err]) = copy(history = history :+ s)
      def ++(s: History.Steps[Err]) = copy(history = history ++ s)
    }

    val ActionName = "Action"

    def checkAround[A](name: String, checks: Check.Around.Composite[Obs, State, Err], omg: OMG)
                      (prepare: ROS => Option[A])
                      (run: A => (String => History.Step[Err], ROS)): OMG =

      prepare(omg.ros) match {
        case Some(a) =>

          // Perform before
          val pre = {
            val b = History.newBuilder[Err]
            b.addEach(checks.befores)(_.check name omg.ros.sos, _.check.test(omg.ros.os))
            b.history()
          }

          if (pre.failed) {
            val children = pre :+ History.Step(ActionName, Result.Skip)
            omg :+ History.parent(name, children)

          } else {

            // Perform around-pre
            val hcs = halfChecks(checks)(omg.ros.os)

            // Perform action
            val (mkStep, ros2) = run(a)
            val step = mkStep(if (pre.isEmpty) name else ActionName)
            if (step.failed) {

              if (pre.isEmpty)
                omg :+ step
              else
                omg :+ History.parent(name, pre :+ step)

            } else {

              // Perform post
              val post = {
                val b = History.newBuilder[Err]
                b.addEach(hcs)(_.check name omg.ros.sos, c => c.check.test(ros2.os, c.before)) // Perform around-post
                b.addEach(checks.afters)(_.check name omg.ros.sos, _.check.test(ros2.os)) // Perform post
                b.addEach(invariantsPoints)(_ name omg.ros.sos, _.test(ros2.os))// Perform invariants
                b.history()
              }

              if (pre.nonEmpty)
                omg :+ History.parent(name, (pre :+ step) ++ post.steps) // TODO inefficient
              else if (post.isEmpty)
                omg :+ step
              else //if (post.failed)
                omg :+ History.Step(name, post.result, History(step.copy(name = ActionName)) ++ post.steps) // TODO need to handle children !!!!!!!!!!!!! Also inefficient
//              else
//                omg :+ History.Step(name, post.result, post) // TODO need to handle children !!!!!!!!!!!!!
            }
          }

          case None =>
            omg :+ History.Step(name, Result.Skip)
      }

    def start(a: A, ros: ROS, history: History[Err] = History.empty) =
      go(vector1(a), OMG(ros, history))

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
            val omg2 =
              checkAround(name, check & invariantsAround, omg)(run)(act =>
                act() match {
                  case Right(f) =>
                    val obs2 = observe(ref)
                    val state2 = f(obs2)
                    val ros2 = ROS(ref, obs2, state2)
                    (History.Step(_, Result.Pass), ros2)
                  case Left(e) =>
                    (History.Step(_, Result.Fail(e)), ros)
                }
              )
            if (omg2.failed)
              omg2
            else
              go(queue.tail, omg2)

          // ==============================================================================
          case Action.Group(nameFn, actionFn, check) =>
            val name = nameFn(ros.sos)
            val omg2 =
              checkAround(name, check & invariantsAround, omg)(actionFn)(children => {
                val r = start(children, ros)
                (History.parent(_, r.history), r.ros)
              })
            if (omg2.failed)
              omg2
            else
              go(queue.tail, omg2)

          // ==============================================================================
          case Action.Composite(actions) =>
            go(queue.tail ++ actions.toVector, omg)
        }
      }

    val finalResult: History[Err] = {
      val ros = ROS(ref, observe(ref), initialState)

      val firstSteps: History[Err] =
        if (invariantsPoints.isEmpty)
          History.empty
        else {
          val children = invariantsPoints.map { i =>
            val name = i.name(ros.sos)
            val result = Result passOrFail i.test(ros.os)
            History.Step(name, result)
          }
          History(History.parent("Initial state.", History(children)))
        }

      firstSteps
        .unlessFailed(start(test.action, ros, _).history
          .unlessFailed(_ :+ History.Step("All pass.", Result.Pass)))
    }
    finalResult
  }

  private def halfChecks[O, S, E](checks: Check.Around[O, S, E])(os: OS[O, S]): Vector[HalfCheck[O, S, E]] =
    checks.dunnos.map { c0 =>
      val c = c0.aux
      val a = c.before(os)
      HalfCheck(c)(a)
    }
}