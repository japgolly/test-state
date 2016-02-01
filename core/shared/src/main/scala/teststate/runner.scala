package teststate

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

// TODO Maybe better: Script | Plan | TestCase
class Test[F[_], Ref, Obs, State, Err](val action: Action[F, Ref, Obs, State, Err],
                                       val invariants: Check[Obs, State, Err],
                                       val observe: Ref => Obs)
                                      (implicit val executionModel: ExecutionModel[F], val recover: Recover[Err]) {
  def run(initialState: State, ref: Ref): F[History[Err]] =
    Runner.run(this)(initialState, ref)

  // TODO add invariants
  // TODO add actions
}
object Test {
  def apply[F[_], Ref, Obs, State, Err](action: Action[F, Ref, Obs, State, Err],
                                        invariants: Check[Obs, State, Err] = Check.empty)
                                       (observe: Ref => Obs)
                                       (implicit executionModel: ExecutionModel[F], recover: Recover[Err]): Test[F, Ref, Obs, State, Err] =
    new Test(action, invariants, observe)
}

final case class Recover[E](apply: Throwable => E) extends AnyVal {
  def recover[A](a: => A, ko: E => A): A =
    try a catch { case t: Throwable => ko(apply(t)) }
}
object Recover {
  implicit val recoverToString: Recover[String] =
    Recover("Caught exception: " + _.toString)

  private val forName = Recover("Name exception: " + _.toString)
  def name(n: => Name): Name =
    forName.recover(n, Name(_))

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
                                     (initialState: State, ref: Ref): F[History[Err]] = {
import test.{executionModel => EM, recover}
import test.observe
    // TODO Catch all exceptions

    type A = Action[F, Ref, Obs, State, Err]
    type HS = History.Steps[Err]
    type ROS = teststate.ROS[Ref, Obs, State]

    val invariantsAround = test.invariants.around
    val invariantsPoints = test.invariants.point.singles

    case class OMG(queue: Vector[A], ros: ROS, history: History[Err]) {
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

    def checkAround[A](name: Name, checks: Check.Around.Composite[Obs, State, Err], collapse: Boolean, omg: OMG)
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

              def addStep(s: History.Step[Err]) =
                omg.copy(ros = ros2, history = omg.history :+ s)

              val step = mkStep(ActionName)
              val collapseIfNoPost = collapse && pre.isEmpty && step.steps.length == 1
              def collapsed = step.steps(0).copy(name = name)
              if (step.failed) {

                if (collapseIfNoPost)
                  addStep(collapsed)
                else
                  addStep(History.parent(name, pre ++ step))

              } else {

                // Post conditions
                val post1 = {
                  val b = History.newBuilder[Err]
                  b.addEach(hcs)(_.check name omg.ros.sos, c => c.check.test(ros2.os, c.before)) // Perform around-post
                  b.addEach(checks.afters)(_.check name omg.ros.sos, _.check.test(ros2.os)) // Perform post
                  b.group(PostName)
                }

                // Check invariants
                val invs = {
                  val b = History.newBuilder[Err]
                  b.addEach(invariantsPoints)(_ name omg.ros.sos, _.test(ros2.os))
                  b.group(InvariantsName)
                }

                val post = post1 ++ invs

                if (collapseIfNoPost && post.isEmpty)
                  addStep(collapsed)
                else
                  addStep(History.parent(name, pre ++ step ++ post))
              }
            }
          }

        case None =>
            EM.pure(omg :+ History.Step(name, Result.Skip))
      }

    def start(a: A, ros: ROS, history: History[Err] = History.empty) =
      go(OMG(vector1(a), ros, history))

    def go(omg: OMG): F[OMG] =
      EM.tailrec(omg)(x => x.queue.isEmpty || x.failed) { omg =>

        def continue(r: F[OMG]): F[OMG] =
          EM.map(r)(_.copy(queue = omg.queue.tail))

        import omg.ros
        omg.queue.head match {

          // ==============================================================================
          case Action.Single(nameFn, run, check) =>
            val name = Recover.name(nameFn(ros.sos))
            val omg2F =
              checkAround(name, check & invariantsAround, true, omg)(run)(act =>
                EM.map(EM.recover(act())) {
                  case Right(f) =>
                    val obs2 = observe(ref)
                    val state2 = f(obs2)
                    val ros2 = ROS(ref, obs2, state2)
                    ((n: String) => History(History.Step(n, Result.Pass)), ros2)
                  case Left(e) =>
                    ((n: String) => History(History.Step(n, Result.Fail(e))), ros)
                }
              )
            continue(omg2F)

          // ==============================================================================
          case Action.Group(nameFn, actionFn, check) =>
            val name = Recover.name(nameFn(ros.sos))
            val omg2F =
              checkAround(name, check & invariantsAround, false, omg)(actionFn)(children => {
                val x =
                EM.map(start(children, ros))(omgC =>
                  ((_: String) => omgC.history, omgC.ros))
                x
              })
            continue(omg2F)

          // ==============================================================================
          case Action.Composite(actions) =>
            EM.pure(omg.copy(queue = omg.queue.tail ++ actions.toVector))
        }
      }

    val finalResult: F[History[Err]] = {
      val ros = ROS(ref, observe(ref), initialState)

      val firstSteps: History[Err] =
        if (invariantsPoints.isEmpty)
          History.empty
        else {
          val children = {
            val b = History.newBuilder[Err]
            b.addEach(invariantsPoints)(_ name ros.sos, _ test ros.os)
            b.history()
          }
          History(History.parent("Initial state.", children))
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
