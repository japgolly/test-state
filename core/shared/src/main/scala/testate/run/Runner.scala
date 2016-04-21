package testate.run

// import acyclic.file
import scala.annotation.tailrec
import scala.collection.mutable
import testate.data._
import testate.typeclass._
import testate.core._
import CoreExports._
import CoreExports2._
import Types.SackE
import Result.{Fail, Skip, Pass}

object Runner {

  trait HalfCheck[O, S, E] {
    type A
    val check: Around.DeltaAux[OS[O, S], E, A]
    val before: Tri[Failure[E], A]
  }

  def HalfCheck[O, S, E, a](_check: Around.DeltaAux[OS[O, S], E, a])(_before: Tri[Failure[E], a]): HalfCheck[O, S, E] =
    new HalfCheck[O, S, E] {
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


  final def foreachSack[I, A](sack: Sack[I, A])(i: I)(err: (Name, Throwable) => Unit)(f: A => Unit): Boolean = {
    var coproductFound = false
    def go(s: Sack[I, A]): Unit =
      s match {
        case Sack.Value(a)        => f(a)
        case Sack.Product(ss)     => ss foreach go
        case Sack.CoProduct(n, p) =>
          coproductFound = true
          Recover.id.attempt(p(i)) match {
            case Right(s) => go(s)
            case Left(e)  => err(Recover.recoverToString.name(n, Some(i)), e.failure)
          }
      }
    go(sack)
    coproductFound
  }

  def foreachSackE[A, B, E](s: SackE[A, B, E])(a: A)(f: NamedError[Failure[E]] Or B => Unit)(implicit r: Recover[E]) =
    foreachSack(s)(a)((n, t) => f(Left(NamedError(n, r(t)))))(f)

  private case class ActionQueue[F[_], R, O, S, E](head: NamedError[Failure[E]] Or Action.Outer[F, R, O, S, E],
                                                   tail: Actions[F, R, O, S, E])

  private case class Progress[F[_], R, O, S, E](queue  : Option[ActionQueue[F, R, O, S, E]],
                                                ros    : ROS[R, O, S],
                                                history: History[Failure[E]]) {
    def failure: Option[Failure[E]] = history.failure
    def failed : Boolean            = history.failed

    def :+(s: History.Step[Failure[E]])  = copy(history = history :+ s)
    def ++(s: History.Steps[Failure[E]]) = copy(history = history ++ s)
    def ++(s: History[Failure[E]])       = copy(history = history ++ s.steps)
  }

  private object Progress {
    def prepareNext[F[_], R, O, S, E](actions: Actions[F, R, O, S, E],
                                      ros    : ROS[R, O, S],
                                      history: History[Failure[E]])(implicit r: Recover[E]): Progress[F, R, O, S, E] = {

      @tailrec
      def queue(subject: Actions[F, R, O, S, E], tail: Actions[F, R, O, S, E]): Option[ActionQueue[F, R, O, S, E]] =
        subject match {
          case Sack.Value(a) =>
            Some(ActionQueue(a, tail))

          case Sack.Product(as) =>
            as.length match {
              case 0 => None
              case 1 => queue(as.head, tail)
              case _ => queue(as.head, Sack.append(Sack.Product(as.tail), tail))
            }

          case Sack.CoProduct(n, p) =>
            r.attempt(p(ros)) match {
              case Right(as) => queue(as, tail)
              case Left(e)   => Some(ActionQueue(Left(NamedError(r.name(n, ros.some), e)), tail))
            }
        }

      Progress(queue(actions, Sack.empty), ros, history)
    }
  }

  private type CheckNE[C[_, _], O, S, E] = NamedError[Failure[E]] Or C[OS[O, S], E]

  final class RefEq[+A <: AnyRef](val value: A) {
    override def hashCode = value.hashCode
    override def equals(x: Any) = x match {
      case y: RefEq[AnyRef] => value eq y.value
      case _                => false
    }
  }

  final class UniqueListBuilder[A <: AnyRef] {
    private val set = mutable.LinkedHashSet.empty[RefEq[A]]

    def +=(a: A): Unit = {
      set add new RefEq(a)
      ()
    }

    def iterator(): Iterator[A] =
      set.iterator.map(_.value)

    def result(): List[A] =
      iterator().toList
  }

  case class UnpackChecks[F[_], O, S, E](befores       : F[Point        [OS[O, S], E]],
                                         deltas        : F[Around.DeltaA[OS[O, S], E]],
                                         aftersA       : F[Point        [OS[O, S], E]],
                                         aftersI       : F[Point        [OS[O, S], E]],
                                         errors        : F[NamedError[Failure[E]]],
                                         coproductFound: Boolean)

  def unpackChecks[O, S, E](invariants: Invariants[O, S, E],
                            arounds   : Arounds[O, S, E],
                            input     : OS[O, S])
                           (implicit r: Recover[E]): UnpackChecks[List, O, S, E] = {

    import Around.{Before, After}

    type Builder[A <: AnyRef] = UniqueListBuilder[A]

    def newBuilder[A <: AnyRef]: Builder[A] =
      new UniqueListBuilder[A]

    def add[A <: AnyRef](b: Builder[A], a: A): Unit =
      b += a

    def result[A <: AnyRef](r: Builder[A]): List[A] =
      r.result()

    val bs = newBuilder[Point        [OS[O, S], E]]
    val ds = newBuilder[Around.DeltaA[OS[O, S], E]]
    val aa = newBuilder[Point        [OS[O, S], E]]
    val ai = newBuilder[Point        [OS[O, S], E]]
    val es = newBuilder[NamedError[Failure[E]]]

    val coproductFoundI =
      foreachSackE(invariants)(input) {
        case Right(Invariant.Point(p)) => add(ai, p)
        case Right(Invariant.Delta(d)) => add(ds, d)
        case Left(e)                   => add(es, e)
      }

    val coproductFoundA =
      foreachSackE(arounds)(input) {
        case Right(Around.Delta(d)        ) => add(ds, d)
        case Right(Around.Point(p, Before)) => add(bs, p)
        case Right(Around.Point(p, After )) => add(aa, p)
        case Left(e)                        => add(es, e)
      }

    UnpackChecks(result(bs), result(ds), result(aa), result(ai), result(es), coproductFoundI || coproductFoundA)
  }

  def run[F[_], R, O, S, E](test: Test[F, R, O, S, E])(initialState: S, ref: => R): F[Report[E]] = {
    val runner = new Runner[F, R, O, S, E]()(test.executionModel, test.recover)
    runner.run(test)(initialState, () => ref)
  }

  @inline implicit class RunnerTriExt[E, A](private val self: Tri[E, A]) extends AnyVal {
    def noCause: Tri[Failure[E], A] =
      self.mapE(Failure NoCause _)
  }
  @inline implicit class RunnerOptionExt[E](private val self: Option[E]) extends AnyVal {
    def noCause: Option[Failure[E]] =
      self.map(Failure NoCause _)
  }
}

/**
  * Internal use only. Not thread-safe (due to mutable stats). Don't expose.
  */
private final class Runner[F[_], R, O, S, E](implicit EM: ExecutionModel[F], recover: Recover[E]) {
  import Runner._

  private type FE   = Failure[E]
  private type H    = History[FE]
  private type ROS  = testate.data.ROS[R, O, S]
  private type Test = testate.run.Test[F, R, O, S, E]
  private type P    = Progress[F, R, O, S, E]

  private def observe(test: Test, ref: R): FE Or O =
    recover.recover(test.observer(ref).leftMap(Failure NoCause _), Left(_))

  private var stats: Stats.Mutable = _

  def run(test: Test)(initialState: S, refFn: () => R): F[Report[E]] =
    EM flatten EM.point {
      stats = new Stats.Mutable
      stats.startTimer()
      val ref = refFn()

      val history: F[H] =
        observe(test, ref) match {

          case Right(obs) =>
            val ros = new ROS(ref, obs, initialState)
            EM.map(subtest(test, Sack.empty, refFn, ros, true))(_.history)

          case Left(e) =>
            val s = History.Step(Observation, Fail(e))
            val h = History(History.parent(InitialState, History(s)))
            EM pure h
        }

      EM.map(history) { h =>
        stats.stopTimer()
        Report(test.name, h, stats.result())
      }
    }

  private def checkAround0(name      : Name,
                           invariants: Invariants[O, S, E],
                           arounds   : Arounds[O, S, E],
                           collapse  : Boolean,
                           p         : P,
                           run       : => F[(H, ROS)]): F[P] = {

    val checksPre = unpackChecks(invariants, arounds, p.ros.os)

    // Perform before
    val pre = {
      val b = History.newBuilder[E](stats)
      checksPre.errors foreach b.addNE
      b.addEach(checksPre.befores)(_.name)(p.ros.sos, _.test(p.ros.os).noCause)
      b.group(PreName)
    }

    if (pre.failed) {
      EM.pure(p :+ History.parent(name, pre))

    } else {

      // Perform around-pre
      val hcs = {
        val b = Vector.newBuilder[HalfCheck[O, S, E]]
        for (d0 <- checksPre.deltas) {
          val d = d0.aux
          val r = recover.attempt(d.before(p.ros.os)).fold[Tri[FE, d.A]](Failed(_), _.noCause)
          b += HalfCheck(d)(r)
        }
        b.result()
      }

      // Perform action
      EM.map(run) { case (step, ros2) =>

        def addStep(s: History.Step[FE]) =
          p.copy(ros = ros2, history = p.history :+ s)

        val collapseIfNoPost = collapse && pre.isEmpty && step.steps.length == 1
        def collapsed = step.steps(0).copy(name = name)
        if (step.failed) {

          if (collapseIfNoPost)
            addStep(collapsed)
          else
            addStep(History.parent(name, pre ++ step))

        } else {

          // TODO This didn't work out as planned - redo unpackChecks stuff
          var checksPostA, checksPostI = checksPre
          if (checksPre.coproductFound) {
            checksPostA = unpackChecks(Sack.empty, arounds, ros2.os)
            checksPostI = unpackChecks(invariants, Sack.empty, ros2.os)
          }

          // Post conditions
          val post1 = {
            val b = History.newBuilder[E](stats)
            checksPostA.errors foreach b.addNE
            b.addEach(hcs)(
              c => c.check.name)(Some(BeforeAfter(p.ros.os, ros2.os)),
              c => c.before.flatMap(a => Tri failedOption c.check.test(ros2.os, a).noCause)) // Perform around-post
            b.addEach(checksPostA.aftersA)(_.name)(ros2.sos, _.test(ros2.os).noCause) // Perform post
            b.group(PostName)
          }

          // Check invariants
          val invs = {
            val b = History.newBuilder[E](stats)
            checksPostI.errors foreach b.addNE
            b.addEach(checksPostI.aftersI)(_.name)(ros2.sos, _.test(ros2.os).noCause)
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
  }

  private def checkAround[A](name      : Name,
                             invariants: Invariants[O, S, E],
                             arounds   : Arounds[O, S, E],
                             collapse  : Boolean,
                             p         : P)
                            (prepare   : ROS => Option[A])
                            (run       : A => F[(Name => H, ROS)]): F[P] =
    prepare(p.ros) match {
      case Some(a) =>
        checkAround0(name, invariants, arounds, collapse, p, EM.map(run(a))(x => (x._1(ActionName), x._2)))
      case None =>
        EM.pure(p :+ History.Step(name, Skip))
    }

  private def subtest(test: Test,
                      nonInitialInvariants: Invariants[O, S, E],
                      refFn: () => R,
                      initROS: ROS,
                      summariseFinalResult: Boolean): F[P] = {

    def start(actions: Actions[F, R, O, S, E], ros: ROS, history: H = History.empty) =
      go(Progress.prepareNext(actions, ros, history))

    def go(p0: P): F[P] =
      EM.tailrec(p0)(_.queue.isEmpty) { p =>
        val queue = p.queue.get
        import p.ros

        val processNextStep: F[P] =
          queue.head match {
            case Right(Action.Outer(nameFn, innerAction, check)) =>

              val name = recover.name(nameFn, ros.some)

              if (p.failed)
                EM.pure(p :+ History.Step(name, Skip))

              else {

                val invariants = nonInitialInvariants & test.invariants
                innerAction match {

                  // ==============================================================================
                  case Action.Single(run) =>
                    checkAround(name, invariants, check, true, p)(run) { act =>

                      def ret(ros: ROS, r: Result[FE]): (Name => H, ROS) =
                        (n => History(vector1(History.Step(n, r))), ros)

                      def rets(ros: ROS, r: Result[FE], hs: History.Step[FE]): (Name => H, ROS) =
                        (n => History(vector1(History.Step(n, r)) :+ hs), ros)

                      stats.actions += 1
                      def act2 = EM.map(act())(_.leftMap(Failure NoCause _)): F[FE Or (O => E Or S)]
                      EM.map(EM.recover(act2)) {

                        case Right(nextStateFn) =>
                          val ref2 = refFn()
                          observe(test, ref2) match {
                            case Right(obs2) =>
                              recover.attempt(nextStateFn(obs2)) match {
                                case Right(Right(state2)) => ret(new ROS(ref2, obs2, state2), Pass)
                                case Right(Left(e))       => rets(ros, Pass, History.Step(Observation, Fail(Failure NoCause e)))
                                case Left(e)              => rets(ros, Pass, History.Step(UpdateState, Fail(e)))
                              }
                            case Left(e) =>
                              rets(ros, Pass, History.Step(Observation, Fail(e)))
                          }

                        case Left(e) => ret(ros, Fail(e))
                      }

                    }

                  // ==============================================================================
                  case Action.Group(actionFn) =>
                    checkAround(name, invariants, check, false, p)(actionFn)(children =>
                      EM.map(start(children, ros))(omgC =>
                        ((_: Name) => omgC.history, omgC.ros))
                    )

                  // ==============================================================================
                  case Action.SubTest(action, subInvariants) =>
                    val omg2F = checkAround0(name, Sack.empty, check, false, p.copy(history = History.empty), {
                      val t = Plan(action, subInvariants).test(test.observer)
                      val subP = subtest(t, invariants, refFn, ros, false)
                      EM.map(subP)(s => (s.history, s.ros))
                    })
                    EM.map(omg2F)(s => s.copy(history = p.history ++ s.history))
                }
            }

            case Left(NamedError(n, e)) =>
              EM.pure(p :+ History.Step(n, Fail(e)))
          }

        EM.map(processNextStep)(p => Progress.prepareNext(queue.tail, p.ros, p.history))
      }

    val finalResult: F[P] = {
      val ros = initROS

      val invariantsPoints = {
        val ps = new UniqueListBuilder[Point[OS[O, S], E]]
        val es = new UniqueListBuilder[NamedError[FE]]
        foreachSackE(test.invariants)(ros.os) {
          case Right(Invariant.Point(p)) => ps += p
          case Right(Invariant.Delta(_)) => ()
          case Left(e)                   => es += e
        }
        type T = CheckNE[Point, O, S, E]
        val pi = ps.iterator().map[T](Right(_))
        val ei = es.iterator().map[T](Left(_))
        (pi ++ ei).toList
      }

      val firstSteps: H =
        if (invariantsPoints.isEmpty)
          History.empty
        else {
          val children = {
            val b = History.newBuilder[E](stats)
            b.addEachNE(invariantsPoints)(_.name)(ros.sos, _.test(ros.os).noCause)
            b.history()
          }
          History(History.parent(InitialState, children))
        }

      val fh: F[P] =
        if (firstSteps.failed)
          EM.pure(Progress[F, R, O, S, E](None, ros, firstSteps))
        else
          start(test.actions, ros, firstSteps)

      EM.map(fh) { omg =>
        import omg.{history => h}

        // Summarise
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
