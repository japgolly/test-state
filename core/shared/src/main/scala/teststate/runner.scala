package teststate

import scala.annotation.tailrec

sealed trait Result[+Err] {
  def failure: Option[Err]
  def +[e >: Err](r: Result[e]): Result[e]
}
object Result {
  def empty[E]: Result[E] = Skip
  case object Pass extends Result[Nothing] {
    override def failure = None
    override def +[e](r: Result[e]): Result[e] = r match {
      case Pass | Skip => Pass
      case Fail(_)     => r
    }
  }
  case object Skip extends Result[Nothing] {
    override def failure = None
    override def +[e](r: Result[e]): Result[e] = r
  }
  case class Fail[+Err](error: Err) extends Result[Err] {
    override def failure = Some(error)
    override def +[e >: Err](r: Result[e]): Result[e] = this
  }
  def passOrFail[E](o: Option[E]): Result[E] =
    o.fold[Result[E]](Pass)(Fail(_))
}

case class Test0[F[_]: ExecutionModel, Ref, Obs, State, Err](action: Action[F, Ref, Obs, State, Err],
                                        invariants: Check[Obs, State, Err] = Check.empty) {
  def observe(f: Ref => Obs) =
    Test(action, f, invariants)
}

case class Test[F[_], Ref, Obs, State, Err](action: Action[F, Ref, Obs, State, Err],
                                       observe: Ref => Obs,
                                       invariants: Check[Obs, State, Err] = Check.empty)
                                      (implicit val executionModel: ExecutionModel[F]){
  def run(initialState: State, ref: Ref): F[History[Err]] =
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

  def run[F[_], Ref, Obs, State, Err](test: Test[F, Ref, Obs, State, Err])
//                               (observe: Ref => Obs)
                               (initialState: State, ref: Ref): F[History[Err]] = {
import test.{executionModel => EM}
import test.observe
    // TODO Catch all exceptions

    type A = Action[F, Ref, Obs, State, Err]
    type HS = History.Steps[Err]
    type ROS = teststate.ROS[Ref, Obs, State]

    val invariantsAround = test.invariants.around
    val invariantsPoints = test.invariants.point.singles

    case class OMG(ros: ROS, history: History[Err]) {
      def failure = history.failure
      def failed = history.failed

      def :+(s: History.Step[Err]) = copy(history = history :+ s)
      def ++(s: History.Steps[Err]) = copy(history = history ++ s)
      def ++(s: History[Err]) = copy(history = history ++ s.steps)
    }

    val ActionName = "Action"
    val PreName = "Pre-conditions"
    val PostName = "Post-conditions"
    val InvariantsName = "Invariants"

    def checkAround[A](name: String, checks: Check.Around.Composite[Obs, State, Err], collapse: Boolean, omg: OMG)
                      (prepare: ROS => Option[A])
                      (run: A => F[(String => History[Err], ROS)]): F[OMG] =

      prepare(omg.ros) match {
        case Some(a) =>

          // Perform before
          val pre = {
            val b = History.newBuilder[Err]
            b.addEach(checks.befores)(_.check name omg.ros.sos, _.check.test(omg.ros.os))
            b.group(PreName)
          }

          if (pre.failed) {
            EM.pure(omg :+ History.parent(name, pre))

          } else {

            // Perform around-pre
            val hcs = halfChecks(checks)(omg.ros.os)

            // Perform action
            val runF = run(a)
            EM.map(runF) { case (mkStep, ros2) =>
              val step = mkStep(ActionName)
              val collapseIfNoPost = collapse && pre.isEmpty && step.steps.length == 1
              def collapsed = step.steps(0).copy(name = name)
              if (step.failed) {

                if (collapseIfNoPost)
                  omg :+ collapsed
                else
                  omg :+ History.parent(name, pre ++ step)

              } else {

                //
                val post1 = {
                  val b = History.newBuilder[Err]
                  b.addEach(hcs)(_.check name omg.ros.sos, c => c.check.test(ros2.os, c.before)) // Perform around-post
                  b.addEach(checks.afters)(_.check name omg.ros.sos, _.check.test(ros2.os)) // Perform post
                  b.group(PostName)
                }

                // Perform invariants
                val invs = {
                  val b = History.newBuilder[Err]
                  b.addEach(invariantsPoints)(_ name omg.ros.sos, _.test(ros2.os))
                  b.group(InvariantsName)
                }

                val post = post1 ++ invs

                if (collapseIfNoPost && post.isEmpty)
                  omg :+ collapsed
                else
                  omg :+ History.parent(name, pre ++ step ++ post)
              }
            }
          }

          case None =>
            EM.pure(omg :+ History.Step(name, Result.Skip))
      }

    def start(a: A, ros: ROS, history: History[Err] = History.empty) =
      go(vector1(a), OMG(ros, history))

//    @tailrec
    def go(queue: Vector[A], omg: OMG): F[OMG] =
      if (queue.isEmpty)
        EM.pure(omg)
      else {
        import omg.ros

        queue.head match {

          // ==============================================================================
          case Action.Single(nameFn, run, check) =>
            val name = nameFn(ros.sos)
            val omg2F =
              checkAround(name, check & invariantsAround, true, omg)(run)(act =>
                EM.map(act()) {
                  case Right(f) =>
                    val obs2 = observe(ref)
                    val state2 = f(obs2)
                    val ros2 = ROS(ref, obs2, state2)
                    ((n: String) => History(History.Step(n, Result.Pass)), ros2)
                  case Left(e) =>
                    ((n: String) => History(History.Step(n, Result.Fail(e))), ros)
                }
              )
            EM.flatMap(omg2F)(omg2 =>
              if (omg2.failed)
                EM.pure(omg2)
              else
                go(queue.tail, omg2)
            )

          // ==============================================================================
          case Action.Group(nameFn, actionFn, check) =>
            val name = nameFn(ros.sos)
            val omg2F =
              checkAround(name, check & invariantsAround, false, omg)(actionFn)(children => {
                EM.map(start(children, ros))(r =>
                  ((_: String) => r.history, r.ros))
              })
            EM.flatMap(omg2F)(omg2 =>
              if (omg2.failed)
                EM.pure(omg2)
              else
                go(queue.tail, omg2)
            )

          // ==============================================================================
          case Action.Composite(actions) =>
            go(queue.tail ++ actions.toVector, omg)
        }
      }

    val finalResult: F[History[Err]] = {
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

      val fh = if (firstSteps.failed)
        EM.pure(firstSteps)
      else
        EM.map(start(test.action, ros, firstSteps))(_.history)

      EM.map(fh)(h =>
        if (h.isEmpty)
          History(History.Step("Nothing to do.", Result.Skip))
        else
          h.result match {
            case Result.Pass    => h :+ History.Step("All pass.", Result.Pass)
            case Result.Skip    => h :+ History.Step("All skipped.", Result.Skip)
            case Result.Fail(_) => h
          }
      )
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
