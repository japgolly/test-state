package teststate.run

// import acyclic.file
import teststate.data._
import teststate.typeclass._
import teststate.core._
import CoreExports._
import Types.SackE
import Result.{Fail, Skip, Pass}

object Runner {

  trait HalfCheck[O, S, Err] {
    type A
    val check: Around.DeltaAux[OS[O, S], Err, A]
    val before: Tri[Err, A]
  }

  def HalfCheck[O, S, Err, a](_check: Around.DeltaAux[OS[O, S], Err, a])(_before: Tri[Err, a]): HalfCheck[O, S, Err] =
    new HalfCheck[O, S, Err] {
      override type A = a
      override val check = _check
      override val before = _before
    }

  val ActionName    : Name = "Action"
  val PreName       : Name = "Pre-conditions"
  val PostName      : Name = "Post-conditions"
  val InvariantsName: Name = "Invariants"

  val Observation  = "Observation"
  val UpdateState  = "Update expected state"
  val InitialState = "Initial state."

  private case class Progress[F[_], R, O, S, E](queue  : Vector[Action[F, R, O, S, E]],
                                                ros    : ROS[R, O, S],
                                                history: History[E]) {
    def failure: Option[E] = history.failure
    def failed : Boolean   = history.failed

    def :+(s: History.Step[E])  = copy(history = history :+ s)
    def ++(s: History.Steps[E]) = copy(history = history ++ s)
    def ++(s: History[E])       = copy(history = history ++ s.steps)
  }

  private type CheckNE[C[_, _], O, S, E] = NamedError[E] Or C[OS[O, S], E]

  def foreachSackE[A, B, E](s: SackE[A, B, E])(a: A)(f: NamedError[E] Or B => Unit)(implicit r: Recover[E]): Unit =
    s.foreach(a)((n, t) => f(Left(NamedError(n, r apply t))))(f)

  case class UnpackChecks[F[_], O, S, E](befores: F[Point        [OS[O, S], E]],
                                         deltas : F[Around.DeltaA[OS[O, S], E]],
                                         aftersA: F[Point        [OS[O, S], E]],
                                         aftersI: F[Point        [OS[O, S], E]],
                                         errors : F[NamedError[E]])

  def unpackChecks[O, S, E](invariants: Invariants[O, S, E],
                            arounds   : Arounds[O, S, E],
                            input     : OS[O, S])
                           (implicit r: Recover[E]): UnpackChecks[List, O, S, E] = {

    import Around.{Before, After, BeforeAndAfter}

    val bs = List.newBuilder[Point[OS[O, S], E]]
    val ds = List.newBuilder[Around.DeltaA[OS[O, S], E]]
    val aa = List.newBuilder[Point[OS[O, S], E]]
    val ai = List.newBuilder[Point[OS[O, S], E]]
    val es = List.newBuilder[NamedError[E]]

    foreachSackE(invariants)(input) {
      case Right(Invariant.Point(p)) => ai += p; ()
      case Right(Invariant.Delta(d)) => ds += d; ()
      case Left(e)                   => es += e; ()
    }

    foreachSackE(arounds)(input) {
      case Right(Around.Delta(d)                ) => ds += d; ()
      case Right(Around.Point(p, BeforeAndAfter)) => bs += p; aa += p; ()
      case Right(Around.Point(p, Before)        ) => bs += p; ()
      case Right(Around.Point(p, After)         ) => aa += p; ()
      case Left(e)                                => es += e; ()
    }

    UnpackChecks(bs.result(), ds.result(), aa.result(), ai.result(), es.result())
  }

  def run[F[_], R, O, S, E](test: Test[F, R, O, S, E])
                           (initialState: S, ref: => R): F[History[E]] = {
    val runner = new Runner[F, R, O, S, E]()(test.content.executionModel, test.content.recover)
    runner.run(test)(initialState, () => ref)
  }
}

private final class Runner[F[_], R, O, S, E](implicit EM: ExecutionModel[F], recover: Recover[E]) {
  import Runner._

  private type H    = History[E]
  private type ROS  = teststate.data.ROS[R, O, S]
  private type Test = teststate.run.Test[F, R, O, S, E]
  private type P    = Progress[F, R, O, S, E]

  private def observe(test: Test, ref: R): E Or O =
    recover.recover(test.observe.apply(ref), Left(_))

  def run(test: Test)(initialState: S, refFn: () => R): F[History[E]] = {
    val ref = refFn()

    observe(test, ref) match {
      case Right(obs) =>
        val ros = new ROS(() => ref, obs, initialState)
        EM.map(subtest(test, refFn, ros, true))(_.history)

      case Left(e) =>
        val s = History.Step(Observation, Fail(e))
        val h = History(History.parent(InitialState, History(s)))
        EM pure h
    }
  }

  // TODO Remove duplicate checks by reference

  private def checkAround[N, A](nameFn    : NameFn[ROS],
                                invariants: Invariants[O, S, E],
                                arounds   : Arounds[O, S, E],
                                collapse  : Boolean,
                                p         : P)
                               (prepare   : ROS => Option[A])
                               (run       : A => F[(Name => H, ROS)]): F[P] = {

    val name = recover.name(nameFn, p.ros.some)

    prepare(p.ros) match {
      case Some(a) =>

        val checks = unpackChecks(invariants, arounds, p.ros.os)

        // Perform before
        val pre = {
          val b = History.newBuilder[E]
          checks.errors foreach b.addNE
          b.addEach(checks.befores)(_.name)(p.ros.sos, _.test(p.ros.os))
          b.group(PreName)
        }

        if (pre.failed) {
          EM.pure(p :+ History.parent(name, pre))

        } else {

          // Perform around-pre
          val hcs = {
            val b = Vector.newBuilder[HalfCheck[O, S, E]]
            for (d0 <- checks.deltas) {
              val d = d0.aux
              val r = recover.attempt(d.before(p.ros.os)).fold(Failed(_), identity)
              b += HalfCheck(d)(r)
            }
            b.result()
          }

          // Perform action
          val runF = run(a)
          EM.map(runF) { case (mkStep, ros2) =>

            def addStep(s: History.Step[E]) =
              p.copy(ros = ros2, history = p.history :+ s)

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
                val b = History.newBuilder[E]
                b.addEach(hcs)(
                  c => c.check.name)(ros2.sos,
                  c => c.before.flatMap(a => Tri failedOption c.check.test(ros2.os, a))) // Perform around-post
                b.addEach(checks.aftersA)(_.name)(ros2.sos, _.test(ros2.os)) // Perform post
                b.group(PostName)
              }

              // Check invariants
              val invs = {
                val b = History.newBuilder[E]
                b.addEach(checks.aftersI)(_.name)(ros2.sos, _.test(ros2.os))
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
        EM.pure(p :+ History.Step(name, Skip))
    }
  }

  private def subtest(test: Test,
                      refFn: () => R,
                      initROS: ROS,
                      summariseFinalResult: Boolean): F[P] = {

    import test.content.invariants

    def start(a: Action[F, R, O, S, E], ros: ROS, history: H = History.empty) =
      go(Progress(vector1(a), ros, history))

    def go(p0: P): F[P] =
      EM.tailrec(p0)(x => x.queue.isEmpty || x.failed) { p =>

        def continue(r: F[P]): F[P] =
          EM.map(r)(_.copy(queue = p.queue.tail))

        import p.ros
        p.queue.head match {

          // ==============================================================================
          case Action.Single(nameFn, run, check) =>
            val omg2F =
              checkAround(nameFn, invariants, check, true, p)(run) { act =>

                def ret(ros: ROS, r: Result[E], hs: History.Steps[E] = Vector.empty) =
                  ((n: Name) => History(History.Step(n, r) +: hs), ros)

                EM.map(EM.recover(act())) {
                  case Right(nextStateFn) =>
                    val ref = refFn()
                    observe(test, ref) match {
                      case Right(obs2) =>
                        recover.attempt(nextStateFn(obs2)) match {
                          case Right(Right(state2)) =>
                            val ros2 = new ROS(() => ref, obs2, state2)
                            ret(ros2, Pass)
                          case Right(Left(e)) =>
                            ret(ros, Pass, vector1(History.Step(Observation, Fail(e))))
                          case Left(e) =>
                            ret(ros, Pass, vector1(History.Step(UpdateState, Fail(e))))
                        }
                      case Left(e) =>
                        ret(ros, Pass, vector1(History.Step(Observation, Fail(e))))
                    }
                  case Left(e) =>
                    ret(ros, Fail(e))
                }

              }
            continue(omg2F)

          // ==============================================================================
          case Action.Group(nameFn, actionFn, check) =>
            val omg2F =
              checkAround(nameFn, invariants, check, false, p)(actionFn)(children =>
                EM.map(start(children, ros))(omgC =>
                  ((_: Name) => omgC.history, omgC.ros))
              )
            continue(omg2F)

          // ==============================================================================
          case Action.SubTest(name, action, subInvariants) =>
            val t = new Test(new TestContent(action, invariants & subInvariants), test.observe)
            val subomg = subtest(t, refFn, ros, false)
            EM.map(subomg)(s =>
              Progress(
                p.queue.tail,
                s.ros,
                p.history ++ History.maybeParent(name(ros.some), s.history)
              ))

          // ==============================================================================
          case Action.Composite(actions) =>
            EM.pure(p.copy(queue = p.queue.tail ++ actions.toVector))
        }
      }

    val finalResult: F[P] = {
      val ros = initROS

      val invariantsPoints = {
        val b = Vector.newBuilder[CheckNE[Point, O, S, E]]
        foreachSackE(invariants)(ros.os) {
          case Right(Invariant.Point(p)) => b += Right(p); ()
          case Right(Invariant.Delta(_)) => ()
          case e@Left(_)                 => b += e; ()
        }
        b.result()
      }

      val firstSteps: H =
        if (invariantsPoints.isEmpty)
          History.empty
        else {
          val children = {
            val b = History.newBuilder[E]
            b.addEachNE(invariantsPoints)(_.name)(ros.sos, _ test ros.os)
            b.history()
          }
          History(History.parent(InitialState, children))
        }

      val fh: F[P] =
        if (firstSteps.failed)
          EM.pure(Progress[F, R, O, S, E](Vector.empty, ros, firstSteps))
        else
          start(test.content.action, ros, firstSteps)

      EM.map(fh) { omg =>
        import omg.{history => h}
        val h2: H =
          if (h.isEmpty)
            History(History.Step("Nothing to do.", Skip))
          else if (summariseFinalResult)
            h.result match {
              case Pass    => h :+ History.Step("All pass.", Pass)
              case Skip    => h :+ History.Step("All skipped.", Skip)
              case Fail(_) => h
            }
          else
            h
        omg.copy(history = h2)
      }
    }

    finalResult
  }

}
