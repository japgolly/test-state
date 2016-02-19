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
                                       val observe: Observe[Ref, Obs, Err])
                                      (implicit val executionModel: ExecutionModel[F], val recover: Recover[Err]) {
  def trans[G[_]: ExecutionModel](t: F ~~> G): Test[G, Ref, Obs, State, Err] =
    new Test(action trans t, invariants, observe)

  def cmapRef[R2](f: R2 => Ref): Test[F, R2, Obs, State, Err] =
    new Test(action cmapRef f, invariants, observe cmapR f)

  def run(initialState: State, ref: => Ref): F[History[Err]] =
    Runner.run(this)(initialState, ref)

  // TODO add invariants
  // TODO add actions
}
object Test {
  def apply[F[_], Ref, Obs, State, Err](action: Action[F, Ref, Obs, State, Err],
                                        invariants: Check[Obs, State, Err] = Check.empty)
                                       (implicit executionModel: ExecutionModel[F], recover: Recover[Err])
      : Observe.Ops[Ref, Obs, Err, Test[F, Ref, Obs, State, Err]] =
    new Observe.Ops[Ref, Obs, Err, Test[F, Ref, Obs, State, Err]](o => new Test(action, invariants, o))
}

final case class Recover[E](apply: Throwable => E) extends AnyVal {
  def recover[A](a: => A, ko: E => A): A =
    try a catch { case t: Throwable => ko(apply(t)) }
  def attempt[A](a: => A): Either[E, A] =
    recover(Right(a), Left(_))
}
object Recover {
  implicit val recoverToString: Recover[String] =
    Recover { t =>
      val o = System.err
      o.println()
      t.printStackTrace(o)
      o.println()
      "Caught exception: " + t.toString
    }

  //private val forName = Recover("Name exception: " + _.toString)
//  def name(n: => Name): Name =
//    Name(forName.recover(n.value, identity))

  def name[A](f: Name.Fn[A], a: Some[A]): Name =
    Name(
      try
        f(a).value
      catch {
        case t: Throwable =>
          try {
            val n = f(None).value
            t.printStackTrace()
            n
          } catch {
            case _: Throwable =>
              "Name exception: " + t
          }
      }
    )
}

object Runner {

  trait HalfCheck[O, S, Err] {
    type A
    val check: Check.Around.DunnoA[O, S, Err, A]
    val before: Either[Err, A]
//    def before_! : A = before.asInstanceOf[Right[Err, A]].b
  }
  def HalfCheck[O, S, Err, a](_check: Check.Around.DunnoA[O, S, Err, a])(_before: Either[Err, a]): HalfCheck[O, S, Err] =
    new HalfCheck[O, S, Err] {
      override type A     = a
      override val check  = _check
      override val before = _before
    }

//  private final case class FEM[F[_], E, A](value: F[Either[E, A]]) extends AnyVal {
//    type M[B] = FEM[F, E, B]
//    type FE[B] = F[Either[E, B]]
//
//    def map[B](f: A => B)(implicit em: ExecutionModel[F]): M[B] =
//      FEM(em.map(value)(_ map f))
//
//    def flatMap[B](f: A => M[B])(implicit em: ExecutionModel[F]): M[B] =
//      FEM(em.flatMap(value) { ea =>
//        (ea match {
//          case Right(a) => f(a).value
//          case l: Left[E, A] => em.pure(l.castRight[B])
//        }): FE[B]
//      })
//  }

  def run[F[_], Ref, Obs, State, Err](test: Test[F, Ref, Obs, State, Err])
                                     (initialState: State, ref: => Ref): F[History[Err]] = {
import test.{executionModel => EM, recover}

    val refFn = () => ref

    def observe(): Either[Err, Obs] =
      recover.recover(test.observe.apply(ref), Left(_))

//    type FEM[B] = Runner.FEM[F, Err, B]

    type A = Action[F, Ref, Obs, State, Err]
    type HS = History.Steps[Err]
    type OS = teststate.OS[Obs, State]
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

    val ActionName : Name = "Action"
    val PreName : Name = "Pre-conditions"
    val PostName : Name = "Post-conditions"
    val InvariantsName : Name = "Invariants"
    val Observation = "Observation"
    val UpdateState = "Update expected state"
    val InitialState = "Initial state."

    def checkAround[A](nameFn: Name.Fn[OS], checks: Check.Around.Composite[Obs, State, Err], collapse: Boolean, omg: OMG)
                      (prepare: ROS => Option[A])
                      (run: A => F[(Name => History[Err], ROS)]): F[OMG] = {

      val name = Recover.name(nameFn, omg.ros.sos)

      prepare(omg.ros) match {
        case Some(a) =>

          // Perform before
          val pre = {
            val b = History.newBuilder[Err]
            b.addEach(checks.befores)(_.check.name)(omg.ros.sos, _.check.test(omg.ros.os))
            b.group(PreName)
          }

          if (pre.failed) {
            EM.pure(omg :+ History.parent(name, pre))

          } else {

            // Perform around-pre
//            var hcsBad = false
            val hcs =
              checks.dunnos.map { c0 =>
                val c = c0.aux
                val a = recover attempt c.before(omg.ros.os)
//                hcsBad ||= a.isLeft
                HalfCheck(c)(a)
              }

            /*
            if (hcsBad) {

              // Around-pre failed
              val c = History(hcs.map(hc => hc.before match {
                case Right(a) => History.Step(hc.check.name(omg.ros.sos), Result.Pass)
                case Left(e) => History.Step(hc.check.name(None), Result Fail e)
              }))
              val g = History.maybeParent("SHIT", c)
              EM.pure(omg :+ History.parent(name, g))

            } else {
            */

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
//                  b.addEach(hcs)(_.check name omg.ros.sos, c => c.check.test(ros2.os, c.before_!)) // Perform around-post
                  b.addEach(hcs)(
                    c => c.check.name)(omg.ros.sos,
                    c => c.before.toOptionLeft(a => c.check.test(ros2.os, a))) // Perform around-post
                  b.addEach(checks.afters)(_.check.name)(omg.ros.sos, _.check.test(ros2.os)) // Perform post
                  b.group(PostName)
                }

                // Check invariants
                val invs = {
                  val b = History.newBuilder[Err]
                  b.addEach(invariantsPoints)(_.name)(omg.ros.sos, _.test(ros2.os))
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
            val omg2F =
              checkAround(nameFn, check & invariantsAround, true, omg)(run) { act =>

                /*
                val xx =
                for {
                  nextStateFn <- FEM(EM.recover(act()))
                  obs2 <- FEM(EM pure observe())
                } yield {
                  val state2 = nextStateFn(obs2)
                  val ros2 = ROS(ref, obs2, state2)
                  ((n: String) => History(History.Step(n, Result.Pass)), ros2)
                }

                EM.map(xx.value) {
                  case Right(a) => a
                  case Left(e) => fail(e)
                }*/

                def ret(ros: ROS, r: Result[Err], hs: History.Steps[Err] = Vector.empty) =
                  ((n: Name) => History(History.Step(n, r) +: hs), ros)

                EM.map(EM.recover(act())) {
                  case Right(nextStateFn) =>
                    observe() match {
                      case Right(obs2) =>
                        recover.attempt(nextStateFn(obs2)) match {
                          case Right(state2) =>
                            val ros2 = new ROS(refFn, obs2, state2)
                            ret(ros2, Result.Pass)
                          case Left(e) =>
                            ret(ros, Result.Pass, vector1(History.Step(UpdateState, Result Fail e)))
                        }
                      case Left(e) =>
                        ret(ros, Result.Pass, vector1(History.Step(Observation, Result Fail e)))
                    }
                  case Left(e) =>
                    ret(ros, Result Fail e)
                }

              }
            continue(omg2F)

          // ==============================================================================
          case Action.Group(nameFn, actionFn, check) =>
            val omg2F =
              checkAround(nameFn, check & invariantsAround, false, omg)(actionFn)(children => {
                val x =
                EM.map(start(children, ros))(omgC =>
                  ((_: Name) => omgC.history, omgC.ros))
                x
              })
            continue(omg2F)

          // ==============================================================================
          case Action.Composite(actions) =>
            EM.pure(omg.copy(queue = omg.queue.tail ++ actions.toVector))
        }
      }

    val finalResult: F[History[Err]] = {
      observe() match {
        case Right(obs) =>
          val ros = new ROS(refFn, obs, initialState)

          val firstSteps: History[Err] =
            if (invariantsPoints.isEmpty)
              History.empty
            else {
              val children = {
                val b = History.newBuilder[Err]
                b.addEach(invariantsPoints)(_.name)(ros.sos, _ test ros.os)
                b.history()
              }
              History(History.parent(InitialState, children))
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
        case Left(e) =>
          val s = History.Step(Observation, Result Fail e)
          val h = History(History.parent(InitialState, History(s)))
          EM pure h
      }
    }

    finalResult
  }
}
