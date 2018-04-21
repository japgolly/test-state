package teststate.run

// import acyclic.file
import scala.annotation.tailrec
import scala.collection.mutable
import teststate.data._
import teststate.typeclass._
import teststate.core._
import CoreExports._
import CoreExports2._
import Types.SackE
import Result.{Fail, Pass, Skip}

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

  val notRightRight: (Any Or (Any Or Any)) => Boolean = {
    case x: Right[_] => x.isLeft
    case _ => true
  }

  val obsAndTestFailed: (Any Or (Any, PostCheckResults[Any])) => Boolean = {
    case Right((_, hh)) => hh.post.failed
    case _ => true
  }

  final def foreachSack[I, A](sack: Sack[I, A])(i: I)(err: (Name, Throwable) => Unit)(f: A => Unit): Boolean = {
    var coproductFound = false
    def go(s: Sack[I, A]): Unit =
      s match {
        case Sack.Value(a)        => f(a)
        case Sack.Product(ss)     => ss foreach go
        case Sack.CoProduct(n, p) =>
          coproductFound = true
          Attempt.id.attempt(p(i)) match {
            case Right(s) => go(s)
            case Left(e)  => err(Attempt.byToString.name(n, Some(i)), e.failure)
          }
      }
    go(sack)
    coproductFound
  }

  def foreachSackE[A, B, E](s: SackE[A, B, E])(a: A)(f: NamedError[Failure[E]] Or B => Unit)(implicit r: Attempt[E]) =
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
                                      history: History[Failure[E]])(implicit r: Attempt[E]): Progress[F, R, O, S, E] = {

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
                                         afters        : F[Point        [OS[O, S], E]],
                                         errors        : F[NamedError[Failure[E]]],
                                         coproductFound: Boolean)

  object UnpackChecks {

    def invariants[O, S, E](invariants: Invariants[O, S, E], input: OS[O, S])
                           (implicit r: Attempt[E]): UnpackChecks[List, O, S, E] = {

      type Builder[A <: AnyRef] = UniqueListBuilder[A]
      def newBuilder[A <: AnyRef]: Builder[A] = new UniqueListBuilder[A]
      def add[A <: AnyRef](b: Builder[A], a: A): Unit = b += a
      def result[A <: AnyRef](r: Builder[A]): List[A] = r.result()

      val ds = newBuilder[Around.DeltaA[OS[O, S], E]]
      val as = newBuilder[Point        [OS[O, S], E]]
      val es = newBuilder[NamedError[Failure[E]]]

      val coproductFound =
        foreachSackE(invariants)(input) {
          case Right(Invariant.Point(p)) => add(as, p)
          case Right(Invariant.Delta(d)) => add(ds, d)
          case Left(e)                   => add(es, e)
        }

      UnpackChecks(List.empty[Point[OS[O, S], E]], result(ds), result(as), result(es), coproductFound)
    }

    def arounds[O, S, E](arounds: Arounds[O, S, E], input: OS[O, S])
                        (implicit r: Attempt[E]): UnpackChecks[List, O, S, E] = {

      import Around.{Before, After}

      type Builder[A <: AnyRef] = UniqueListBuilder[A]
      def newBuilder[A <: AnyRef]: Builder[A] = new UniqueListBuilder[A]
      def add[A <: AnyRef](b: Builder[A], a: A): Unit = b += a
      def result[A <: AnyRef](r: Builder[A]): List[A] = r.result()

      val bs = newBuilder[Point        [OS[O, S], E]]
      val ds = newBuilder[Around.DeltaA[OS[O, S], E]]
      val as = newBuilder[Point        [OS[O, S], E]]
      val es = newBuilder[NamedError[Failure[E]]]

      val coproductFound =
        foreachSackE(arounds)(input) {
          case Right(Around.Delta(d))         => add(ds, d)
          case Right(Around.Point(p, Before)) => add(bs, p)
          case Right(Around.Point(p, After))  => add(as, p)
          case Left(e)                        => add(es, e)
        }

      UnpackChecks(result(bs), result(ds), result(as), result(es), coproductFound)
    }
  }

  final case class PreCheck[+A](pre: A, invariants: A)
  final case class PostCheckResults[+A](post: History[A], invariants: History[A]) {
    def both: History[A] =
      post ++ invariants
  }
  object PostCheckResults {
    val empty = apply(History.empty, History.empty)
  }

  def run[F[_], R, O, S, E](test: Test[F, R, O, S, E])(initialState: S, refFn: () => R): F[Report[E]] = {
    val runner = new Runner[F, R, O, S, E](test.retryPolicy)(test.executionModel, test.attempt)
    runner.run(test)(initialState, refFn)
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
private final class Runner[F[_], R, O, S, E](retryPolicy: Retry.Policy)
                                            (implicit EM: ExecutionModel[F], attempt: Attempt[E]) {
  import Runner._

  private type FE   = Failure[E]
  private type H    = History[FE]
  private type HH   = PostCheckResults[FE]
  private type ROS  = teststate.data.ROS[R, O, S]
  private type Test = teststate.run.Test[F, R, O, S, E]
  private type P    = Progress[F, R, O, S, E]

  // TODO Ref result should be in an F
  // TODO Observer result should be in an F

  private def withRetry[A](impureRun: () => A)(failed: A => Boolean)
                          (implicit stats: Stats.Mutable): F[A] =
    _withRetryF(impureRun())(EM.point(impureRun()), failed)

  private def withRetryF[A](impureRun: => F[A])(failed: A => Boolean)
                           (implicit stats: Stats.Mutable): F[A] =
    EM.flatMap(impureRun)(_withRetryF(_)(impureRun, failed))

  private def withRetryF1[A](firstAttempt: => F[A], repeatAttempts: => F[A])(failed: A => Boolean)
                            (implicit stats: Stats.Mutable): F[A] =
    EM.flatMap(firstAttempt)(_withRetryF(_)(repeatAttempts, failed))

  private def _withRetryF[A](initialA: A)(retryF: => F[A], failed: A => Boolean)
                            (implicit stats: Stats.Mutable): F[A] = {
    def retryWithStats = EM.flatMap(EM.point(stats.retries += 1))(_ => retryF)
    retryPolicy.retryI(initialA)(failed, retryWithStats)
  }

  private def observe(test: Test, ref: R): F[FE Or O] =
    withRetry[FE Or O](() => observeNoRetry(test, ref))(_.isLeft)

  private def observeNoRetry(test: Test, ref: R): FE Or O =
    attempt.recover(test.observer(ref).leftMap(Failure NoCause _), Left(_))

  private implicit var stats: Stats.Mutable = _

  def run(test: Test)(initialState: S, refFn: () => R): F[Report[E]] =
    EM flatten EM.point {
      stats = new Stats.Mutable
      stats.startTimer()

      val refFnWithRetry: () => R =
        () => retryPolicy.unsafeRetryOnException(refFn(), {
          stats.retries += 1
          refFn()
        })

      val ref = refFnWithRetry()

      val history: F[H] =
        EM.flatMap(observe(test, ref)) {

          case Right(obs) =>
            val ros = new ROS(ref, obs, initialState)
            EM.map(subtest(test, Sack.empty, refFnWithRetry, ros, true))(_.history)
            // TODO Using refFnWithRetry is the easy answer for now but it's incorrect
            // It should be F[Error \/ Ref] without retry so that the get-ref-and-observe blocks
            // have a proper retry that covers ref failures AND obs failures

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

  private def checkAround[A](name      : Name,
                             invariants: Invariants[O, S, E],
                             arounds   : Arounds[O, S, E],
                             collapse  : Boolean,
                             reObserve : () => F[FE Or (R, O)],
                             p         : P)
                            (prepare   : ROS => Option[A])
                            (run       : (A, PostCheckFn) => F[(Name => H, ROS, HH)]): F[P] =
    prepare(p.ros) match {
      case Some(a) =>
        val runFn = (y: PostCheckFn) => EM.map(run(a, y))(x => (x._1(ActionName), x._2, x._3))
        checkAround0(name, invariants, arounds, collapse, reObserve, p, runFn)
      case None =>
        EM.pure(p :+ History.Step(name, Skip))
    }

  /** First arg is step.failed */
  private type PostCheckFn = Boolean => ROS => HH

  private def checkAround0(name      : Name,
                           invariants: Invariants[O, S, E],
                           arounds   : Arounds[O, S, E],
                           collapse  : Boolean,
                           reObserve : () => F[FE Or (R, O)],
                           p0        : P,
                           run       : PostCheckFn => F[(H, ROS, HH)]): F[P] = {

    // Perform before
    def preChecksFn(p: P): (P, PreCheck[H], PreCheck[UnpackChecks[List, O, S, E]]) = {
      val checksPreI = UnpackChecks.invariants(invariants, p.ros.os)
      val checksPreA = UnpackChecks.arounds(arounds, p.ros.os)
      val checksPre = PreCheck(checksPreA, checksPreI)
      val bI = History.newBuilder[E](stats)
      val bA = History.newBuilder[E](stats)
      checksPreA.errors foreach bA.addNE
      checksPreI.errors foreach bI.addNE
      bA.addEach(checksPreA.befores)(_.name)(p.ros.sos, _.test(p.ros.os).noCause)
      bI.addEach(checksPreI.befores)(_.name)(p.ros.sos, _.test(p.ros.os).noCause)
      val pre = PreCheck(bA.group(PreName), bI.group(PreName))
      (p, pre, checksPre)
    }

    def preChecksOnRetryF =
      EM.map(reObserve()) {
        case Right((r, o)) => preChecksFn(p0.copy(ros = ROS(r, o, p0.ros.state)))
        case Left(_) => preChecksFn(p0)
      }

    def preF = withRetryF1(EM.point(preChecksFn(p0)), preChecksOnRetryF)(_._2.pre.failed)

    EM.flatMap(preF) { preResults =>
      val p = preResults._1
      val pre = preResults._2.pre ++ preResults._2.invariants
      val checksPre = preResults._3

      if (pre.failed) {
        EM.pure(p :+ History.parent(name, pre))

      } else {

        def runHalfChecks(deltas: Traversable[Around.DeltaA[OS[O, S], E]],
                          prepend: Vector[HalfCheck[O, S, E]] = Vector.empty): Vector[HalfCheck[O, S, E]] = {
          var b = prepend
          for (d0 <- deltas) {
            val d = d0.aux
            val r = attempt.attempt(d.before(p.ros.os)).fold[Tri[FE, d.A]](Failed(_), _.noCause)
            b :+= HalfCheck(d)(r)
          }
          b
        }

        // Perform around-pre
        val halfChecksI: Vector[HalfCheck[O, S, E]] =
          runHalfChecks(checksPre.invariants.deltas)

        val postChecksFn: PostCheckFn =
          failed =>
            if (failed)
              _ => PostCheckResults.empty
            else
              ros => {
                val halfChecks = runHalfChecks(checksPre.pre.deltas, halfChecksI)
                postChecks(halfChecks, checksPre, invariants, arounds, p, ros)
              }

        // Perform action
        EM.map(run(postChecksFn)) {
          case (step, ros2, hh) =>
            val collapseIfNoPost = collapse && pre.isEmpty && step.steps.length == 1
            def collapsed        = step.steps(0).copy(name = name)
            val result: History.Step[FE] =
              if (step.failed) {
                if (collapseIfNoPost)
                  collapsed
                else
                  History.parent(name, pre ++ step)
              } else {
                val post = hh.both
                if (collapseIfNoPost && post.isEmpty)
                  collapsed
                else
                  History.parent(name, pre ++ step ++ post)
              }
            p.copy(ros = ros2, history = p.history :+ result)
        }
      }
    }
  }

  private def postChecks(hcs       : Vector[HalfCheck[O, S, E]],
                         checksPre : PreCheck[UnpackChecks[List, O, S, E]],
                         invariants: Invariants[O, S, E],
                         arounds   : Arounds[O, S, E],
                         p         : P,
                         ros2      : ROS): HH = {

    // TODO This didn't work out as planned - redo unpackChecks stuff
    var checksPostA = checksPre.pre
    var checksPostI = checksPre.invariants
    if (checksPostA.coproductFound || checksPostI.coproductFound) {
      checksPostA = UnpackChecks.arounds(arounds, ros2.os)
      checksPostI = UnpackChecks.invariants(invariants, ros2.os)
    }

    // Post conditions
    val post = {
      val b = History.newBuilder[E](stats)
      checksPostA.errors foreach b.addNE
      b.addEach(hcs)(
        c => c.check.name)(Some(BeforeAfter(p.ros.os, ros2.os)),
        c => c.before.flatMap(a => Tri failedOption c.check.test(ros2.os, a).noCause)) // Perform around-post
      b.addEach(checksPostA.afters)(_.name)(ros2.sos, _.test(ros2.os).noCause) // Perform post
      b.group(PostName)
    }

    // Check invariants
    val invs = {
      val b = History.newBuilder[E](stats)
      checksPostI.errors foreach b.addNE
      b.addEach(checksPostI.afters)(_.name)(ros2.sos, _.test(ros2.os).noCause)
      b.group(InvariantsName)
    }

    PostCheckResults(post, invs)
  }

  private def subtest(test: Test,
                      nonInitialInvariants: Invariants[O, S, E],
                      refFn: () => R,
                      initROS: ROS,
                      summariseFinalResult: Boolean): F[P] = {

    def start(actions: Actions[F, R, O, S, E], ros: ROS, history: H = History.empty) =
      go(Progress.prepareNext(actions, ros, history))

    def go(p0: P): F[P] =
      EM.tailrecA(p0)(_.queue.isEmpty) { p =>
        val queue = p.queue.get
        import p.ros

        val processNextStep: F[P] =
          queue.head match {
            case Right(Action.Outer(nameFn, innerAction, check)) =>

              val name = attempt.name(nameFn, ros.some)

              if (p.failed)
                EM.pure(p :+ History.Step(name, Skip))

              else {

                val reObserve: () => F[FE Or (R, O)] =
                  () => EM.point {
                    val ref = refFn()
                    observeNoRetry(test, ref).map((ref, _))
                  }

                val invariants = nonInitialInvariants & test.invariants
                innerAction match {

                  // ==============================================================================
                  case Action.Single(prepare) =>
                    checkAround(name, invariants, check, true, reObserve, p)(prepare) { (act, postChk) =>
                      stats.actions += 1
                      def act2: F[FE Or (O => E Or S)] = EM.map(act())(_.leftMap(Failure NoCause _))
                      def act3: F[FE Or (O => E Or S)] = EM.recover(act2)
                      def act4: F[FE Or (O => E Or S)] = withRetryF(act3)(_.isLeft)
                      def fail(f: Name => H) = (f, ros, PostCheckResults.empty)
                      EM.flatMap(act4) {
                        case Right(nextStateFn) =>
                          EM.map(observeAndTestWithRetry(refFn, test, nextStateFn, postChk(false))) {
                            case Right((ros2, hh)) => (histPassFn, ros2, hh)
                            case Left((where, fe)) => fail(hist2Fn(Pass, History.Step(where, Fail(fe))))
                          }
                        case Left(e) =>
                          EM pure fail(hist1Fn(Fail(e)))
                      }
                    }

                  // ==============================================================================
                  case Action.Group(actionFn) =>
                    checkAround(name, invariants, check, false, reObserve, p)(actionFn)((children, postCheckFn) =>
                      EM.flatMap(start(children, ros))(childResults => {

                        val runPostChecks: ROS => (ROS, HH) = {
                          val f = postCheckFn(childResults.history.failed)
                          ros2 => (ros2, f(ros2))
                        }

                        val initialPostCheck: (ROS, HH) =
                          runPostChecks(childResults.ros)

                        def reObserveAndPostCheck: F[(ROS, HH)] =
                          EM.map(reObserve()) {
                            case Right((r, o)) => runPostChecks(ROS(r, o, ros.state))
                            case Left(_)       => initialPostCheck
                          }

                        def postCheckWithRetry: F[(ROS, HH)] =
                          _withRetryF(initialPostCheck)(reObserveAndPostCheck, _._2.post.failed)

                        EM.map(postCheckWithRetry)(x => ((_: Name) => childResults.history, x._1, x._2))
                      })
                    )

                  // ==============================================================================
                  case Action.SubTest(action, subInvariants) =>
                    val runFn = (postChks: PostCheckFn) => {
                      val t = Plan(action, subInvariants).test(test.observer)
                      val subP = subtest(t, invariants, refFn, ros, false)
                      EM.map(subP)(s => (s.history, s.ros, postChks(s.history.failed)(s.ros)))
                    }
                    val omg2F = checkAround0(
                      name,
                      Sack.empty,
                      check,
                      false,
                      reObserve,
                      p.copy(history = History.empty),
                      runFn)
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

  private def hist1Fn(r: Result[FE]): Name => H =
    n => History(vector1(History.Step(n, r)))

  private val histPassFn = hist1Fn(Pass)

  private def hist2Fn(r: Result[FE], hs: History.Step[FE]): Name => H =
    n => History(vector1(History.Step(n, r)) :+ hs)

  private def observeAndTestWithRetry(refFn: () => R, test: Test,
                                      nextStateFn: O => E Or S, postChecks: ROS => HH)
                                      : F[(String, FE) Or (ROS, HH)] = {

    def withoutRetry: F[(String, FE) Or (ROS, HH)] = {
      val ref = refFn()
      EM.flatMap(observe(test, ref)) {
        case Right(obs) =>
          val updateStateF: F[Failure.WithCause[E] Or (E Or S)] =
            withRetry(() => attempt.attempt(nextStateFn(obs)))(notRightRight)
          EM.map(updateStateF) {
            case Right(Right(state)) =>
              val ros = ROS(ref, obs, state)
              Right((ros, postChecks(ros)))
            case Right(Left(e)) => Left((UpdateState, Failure NoCause e))
            case Left(fe) => Left((UpdateState, fe))
          }
        case Left(e) => EM pure Left((Observation, e))
      }
    }

    withRetryF(withoutRetry)(obsAndTestFailed)
  }
}
