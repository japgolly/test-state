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
class TestContent[F[_], Ref, Obs, State, Err](val action: Action[F, Ref, Obs, State, Err],
                                              val invariants: Check[Obs, State, Err])
                                             (implicit val executionModel: ExecutionModel[F], val recover: Recover[Err]) {
  def trans[G[_]: ExecutionModel](t: F ~~> G): TestContent[G, Ref, Obs, State, Err] =
    new TestContent(action trans t, invariants)

  def cmapRef[R2](f: R2 => Ref): TestContent[F, R2, Obs, State, Err] =
    new TestContent(action cmapRef f, invariants)

  def comapRef[R2](f: R2 => Either[Err, Ref]): TestContent[F, R2, Obs, State, Err] =
    new TestContent(action pmapRef f, invariants)

  def pmapO[OO](f: Obs => OO)(g: OO => Either[Err, Obs]): TestContent[F, Ref, OO, State, Err] =
    new TestContent[F, Ref, OO, State, Err](
      action.pmapO(g),
      invariants.pmapO(g))

  def cmapS[SS](s: SS => State, su: (SS, State) => SS): TestContent[F, Ref, Obs, SS, Err] =
    new TestContent(
      action.unzoomS(s, su),
      invariants.cmapS(s))

  def mapE[E](f: Err => E): TestContent[F, Ref, Obs, State, E] =
    new TestContent(
      action mapE f,
      invariants mapE f)(executionModel, recover map f)

  def addInvariants(i: Check[Obs, State, Err]): TestContent[F, Ref, Obs, State, Err] =
    new TestContent(action, invariants & i)

  def addCheck(c: Check.Around[Obs, State, Err]): TestContent[F, Ref, Obs, State, Err] =
    new TestContent(action addCheck c, invariants)

  def asAction(name: NameFn[OS[Obs, State]]) = Action.SubTest(name, action, invariants)

  def observe(f: Ref => Obs) =
    observeTry(r => Right(f(r)))

  def observeTry(f: Ref => Either[Err, Obs]) =
    new Test(this, Observe(f))
}

class Test[F[_], Ref, Obs, State, Err](val content: TestContent[F, Ref, Obs, State, Err],
                                       val observe: Observe[Ref, Obs, Err]) {
  def trans[G[_]: ExecutionModel](t: F ~~> G): Test[G, Ref, Obs, State, Err] =
    new Test(content trans t, observe)

  def cmapRef[R2](f: R2 => Ref): Test[F, R2, Obs, State, Err] =
    new Test(content cmapRef f, observe cmapR f)

  def comapRef[R2](f: R2 => Either[Err, Ref]): Test[F, R2, Obs, State, Err] =
    new Test(content comapRef f, observe comapR f)

//  final def cmapO[X](g: X => O)(implicit em: ExecutionModel[F]): This[F, Ref, X, S, Err] =
//    mapOS(g, identity, (_, s) => s)

//  final def unzoomS[SS](s: SS => State, su: (SS, State) => SS)(implicit em: ExecutionModel[F]) = {
//    val i: Obs => Obs = identity
//    mapOS(i, i, s, su)
//  }

  // TODO This mapping of O is a bit useless. Probably better to think of two types of composition over everything:
  // Product and coproduct
//  def mapOS[OO, SS](o: OO => Option[Obs], o2: Obs => OO, s: SS => State, su: (SS, State) => SS): Test[F, Ref, OO, SS, Err] =
//    new Test(
//      action.mapOS(o, s, su),
//      invariants.cmap(o, s),
//      observe mapO o2)

  def pmapO[OO](f: Obs => OO)(g: OO => Either[Err, Obs]): Test[F, Ref, OO, State, Err] =
    new Test(content.pmapO(f)(g), observe mapO f)

  def cmapS[SS](s: SS => State, su: (SS, State) => SS): Test[F, Ref, Obs, SS, Err] =
    new Test(content.cmapS(s, su), observe)

  def mapE[E](f: Err => E): Test[F, Ref, Obs, State, E] =
    new Test(content mapE f, observe mapE f)

  def run(initialState: State, ref: => Ref): F[History[Err]] =
    Runner.run(this)(initialState, ref)

//  def addCheck(c: Check.Around[Obs, State, Err]): Test[F, Ref, Obs, State, Err] =
//    new Test(content addCheck c, invariants, observe)
}




object Test {
  def apply[F[_], Ref, Obs, State, Err](action: Action[F, Ref, Obs, State, Err],
                                        invariants: Check[Obs, State, Err] = Check.empty)
                                       (implicit em: ExecutionModel[F], recover: Recover[Err]) =
    new TestContent(action, invariants)(em, recover)
}

final case class Recover[E](apply: Throwable => E) extends AnyVal {
  def recover[A](a: => A, ko: E => A): A =
    try a catch { case t: Throwable => ko(apply(t)) }
  def attempt[A](a: => A): Either[E, A] =
    recover(Right(a), Left(_))
  def map[EE](f: E => EE): Recover[EE] =
    Recover(f compose apply)
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

  def name[A](f: NameFn[A], a: Some[A]): Name =
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
import test.content.{executionModel => EM, recover}

    val refFn = () => ref

    type A = Action[F, Ref, Obs, State, Err]
    type HS = History.Steps[Err]
    type OS = teststate.OS[Obs, State]
    type ROS = teststate.ROS[Ref, Obs, State]
    type Test = teststate.Test[F, Ref, Obs, State, Err]

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

    def observe(test: Test): Either[Err, Obs] =
      recover.recover(test.observe.apply(ref), Left(_))

    def subtest(test: Test, initROS: ROS, summariseFinalResult: Boolean): F[OMG] = {

    val invariantsAround = test.content.invariants.around
    val invariantsPoints = test.content.invariants.point.singles

    def checkAround[A](nameFn: NameFn[OS], checks: Check.Around.Composite[Obs, State, Err], collapse: Boolean, omg: OMG)
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
            val hcs = checks.dunnos.map { c0 =>
                val c = c0.aux
                val a = recover attempt c.before(omg.ros.os)
                HalfCheck(c)(a.flatten)
              }

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
                    c => c.check.name)(ros2.sos,
                    c => c.before.toOptionLeft(a => c.check.test(ros2.os, a))) // Perform around-post
                  b.addEach(checks.afters)(_.check.name)(ros2.sos, _.check.test(ros2.os)) // Perform post
                  b.group(PostName)
                }

                // Check invariants
                val invs = {
                  val b = History.newBuilder[Err]
                  b.addEach(invariantsPoints)(_.name)(ros2.sos, _.test(ros2.os))
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

                def ret(ros: ROS, r: Result[Err], hs: History.Steps[Err] = Vector.empty) =
                  ((n: Name) => History(History.Step(n, r) +: hs), ros)

                EM.map(EM.recover(act())) {
                  case Right(nextStateFn) =>
                    observe(test) match {
                      case Right(obs2) =>
                        recover.attempt(nextStateFn(obs2)) match {
                          case Right(Right(state2)) =>
                            val ros2 = new ROS(refFn, obs2, state2)
                            ret(ros2, Result.Pass)
                          case Right(Left(e)) =>
                            ret(ros, Result.Pass, vector1(History.Step(Observation, Result Fail e)))
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
              checkAround(nameFn, check & invariantsAround, false, omg)(actionFn)(children =>
                EM.map(start(children, ros))(omgC =>
                  ((_: Name) => omgC.history, omgC.ros))
              )
            continue(omg2F)

          // ==============================================================================
          case Action.SubTest(name, action, invars) =>
            val t = new Test(new TestContent(action, test.content.invariants & invars), test.observe)
            val subomg = subtest(t, ros, false)
            EM.map(subomg)(s =>
              OMG(
                omg.queue.tail,
                s.ros,
                omg.history ++ History.maybeParent(name(ros.sos), s.history)
              ))

          // ==============================================================================
          case Action.Composite(actions) =>
            EM.pure(omg.copy(queue = omg.queue.tail ++ actions.toVector))
        }
      }

    val finalResult: F[OMG] = {
          val ros = initROS

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

          val fh: F[OMG] =
            if (firstSteps.failed)
              EM.pure(OMG(Vector.empty, ros, firstSteps))
            else
              start(test.content.action, ros, firstSteps)

          EM.map(fh) { omg =>
            import omg.{history => h}
            val h2: History[Err] =
              if (h.isEmpty)
                History(History.Step("Nothing to do.", Result.Skip))
              else if (summariseFinalResult)
                h.result match {
                  case Result.Pass    => h :+ History.Step("All pass.", Result.Pass)
                  case Result.Skip    => h :+ History.Step("All skipped.", Result.Skip)
                  case Result.Fail(_) => h
                }
              else
                h
            omg.copy(history = h2)
          }
    }

    finalResult
    }

    observe(test) match {
      case Right(obs) =>
        val ros = new ROS(refFn, obs, initialState)
        EM.map(subtest(test, ros, true))(_.history)

      case Left(e) =>
        val s = History.Step(Observation, Result Fail e)
        val h = History(History.parent(InitialState, History(s)))
        EM pure h
    }

  }
}
