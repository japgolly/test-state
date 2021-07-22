package teststate.run

import scala.annotation.tailrec
import scala.collection.mutable
import teststate.core.CoreExports._
import teststate.core.Types.SackE
import teststate.core._
import teststate.data.Result.{Fail, Pass, Skip}
import teststate.data._
import teststate.typeclass._

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
    case Right((_, hh)) => hh.retry
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
          ErrorHandler.id.attempt(p(i)) match {
            case Right(s) => go(s)
            case Left(e)  => err(ErrorHandler.byToString.name(n, Some(i)), e.failure)
          }
      }
    go(sack)
    coproductFound
  }

  def foreachSackE[A, B, E](s: SackE[A, B, E])(a: A)(f: NamedError[Failure[E]] Or B => Unit)(implicit r: ErrorHandler[E]) =
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
    @tailrec
    def prepareNext[F[_], R, O, S, E](actions: Actions[F, R, O, S, E],
                                      ros    : ROS[R, O, S],
                                      history: History[Failure[E]])(implicit r: ErrorHandler[E]): Progress[F, R, O, S, E] = {

      type As = Actions[F, R, O, S, E]
      type Ret = Name Or (NamedError[Failure[E]] Or Action.Outer[F, R, O, S, E])

      @tailrec
      def queue(subject: As, tail: As): Option[(Ret, As)] =
        subject match {
          case Sack.Value(a) =>
            Some((Right(a), tail))

          case Sack.Product(as) =>
            as.length match {
              case 0 if tail.isEmpty => None
              case 0                 => queue(tail, Sack.empty)
              case 1                 => queue(as.head, tail)
              case _                 => queue(as.head, Sack.append(Sack.Product(as.tail), tail))
            }

          case Sack.CoProduct(n, p) =>
            def name = r.name(n, None)
            if (history.failed) {
              Some((Right(Right(Action.Outer.skip(name))), tail))
            } else
              r.attempt(p(ros)) match {
                case Right(as) =>
                  if (as.isEmpty) {
                    val name2 = r.name(n, ros.some)
                    Some((Left(name2), tail))
                  } else
                    queue(as, tail)
                case Left(e) =>
                  Some((Right(Left(NamedError(r.name(n, ros.some), e))), tail))
              }
        }

      queue(actions, Sack.empty) match {
        case Some((Right(result), tail)) => Progress(Some(ActionQueue(result, tail)), ros, history)
        case None                        => Progress(None, ros, history)
        case Some((Left(name), tail))    => prepareNext(tail, ros, history :+ History.Step(name, Result.Pass))
      }
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
                           (implicit r: ErrorHandler[E]): UnpackChecks[List, O, S, E] = {

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
                        (implicit r: ErrorHandler[E]): UnpackChecks[List, O, S, E] = {

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
    def retry: Boolean =
      post.failed || invariants.failed
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

  val retryInvariants: Any Or (Any, History[Any]) => Boolean = {
    case Right((_, h)) => h.failed
    case _ => true
  }

  val retryCheckAround0: ActionResult.PreCheckFail[Any] Or Any => Boolean = {
    case Left(x) => x.retry
    case _ => false
  }

  sealed trait ActionResultF[+V, +A, +P, +FE] {
    def retry = false
  }
  object ActionResult {
    case class Success[+V, +A](s: V, a: A) extends ActionResultF[V, A, Nothing, Nothing]
    case object Skip extends ActionResultF[Nothing, Nothing, Nothing, Nothing]
    case class PreCheckFail[+P](progress: P, override val retry: Boolean) extends ActionResultF[Nothing, Nothing, P, Nothing]
    case class ActionFail[+A, +FE](failure: FE, a: A) extends ActionResultF[Nothing, A, Nothing, FE] {
      override def retry = true
    }
    case class ObsFail[+A, +FE](actionFailure: Option[FE], obsFailure: FE, a: A) extends ActionResultF[Nothing, A, Nothing, FE] {
      override def retry = true
    }
  }

}

/**
  * Internal use only. Not thread-safe (due to mutable stats). Don't expose.
  */
private final class Runner[F[_], R, O, S, E](retryPolicy: Retry.Policy)
                                            (implicit EM: ExecutionModel[F], attempt: ErrorHandler[E]) {
  import Runner.{ActionResultF => _, _}

  private type FE   = Failure[E]
  private type H    = History[FE]
  private type HH   = PostCheckResults[FE]
  private type ROS  = teststate.data.ROS[R, O, S]
  private type Test = teststate.run.Test[F, R, O, S, E]
  private type P    = Progress[F, R, O, S, E]

  private type ActionResult[+V, +A] = Runner.ActionResultF[V, A, P, FE]

  // TODO Ref result should be in an F
  // TODO Observer result should be in an F

  private def withRetry[A](scope: Retry.Scope, impureRun: () => A)(failed: A => Boolean)
                          (implicit stats: Stats.Mutable): F[A] =
    _withRetryF(scope, impureRun())(EM.point(impureRun()), failed)

  private def withRetryF[A](scope: Retry.Scope, impureRun: => F[A])(failed: A => Boolean)
                           (implicit stats: Stats.Mutable): F[A] =
    EM.flatMap(impureRun)(_withRetryF(scope, _)(impureRun, failed))

  private def withRetryF1[A](scope: Retry.Scope, firstAttempt: => F[A], repeatAttempts: => F[A])(failed: A => Boolean)
                            (implicit stats: Stats.Mutable): F[A] =
    EM.flatMap(firstAttempt)(_withRetryF(scope, _)(repeatAttempts, failed))

  private def withRetryF1I[A](scope: Retry.Scope, firstAttempt: => F[A])(repeatAttempts: A => F[A], failed: A => Boolean)
                            (implicit stats: Stats.Mutable): F[A] =
    EM.flatMap(firstAttempt)(a0 => _withRetryF(scope, a0)(repeatAttempts(a0), failed))

  private def _withRetryF[A](scope: Retry.Scope, initialA: A)(retryF: => F[A], failed: A => Boolean)
                            (implicit stats: Stats.Mutable): F[A] = {
    def retryWithStats = EM.flatMap(EM.point(stats.retries += 1))(_ => retryF)
    retryPolicy.retryI(scope, initialA)(failed, retryWithStats)
  }

  private def observeWithRetry(test: Test, ref: R): F[FE Or O] =
    withRetry[FE Or O](Retry.Scope.Observation, () => observeNoRetry(test, ref))(_.isLeft)

  private def observeNoRetry(test: Test, ref: R): FE Or O =
    attempt.recover(test.observer(ref).leftMap(Failure NoCause _), Left(_))

  private implicit var stats: Stats.Mutable = _

  def run(test: Test)(initialState: S, refFn: () => R): F[Report[E]] =
    EM flatten EM.point {
      stats = new Stats.Mutable
      stats.startTimer()

      val refFnWithRetry: () => R =
        () => retryPolicy.unsafeRetryOnException(Retry.Scope.Reference, refFn(), {
          stats.retries += 1
          refFn()
        })

      val ref = refFnWithRetry()

      val history: F[H] =
        EM.flatMap(observeWithRetry(test, ref)) {

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

  /** First arg is step.failed */
  private type PostCheckFn = Boolean => ROS => HH

  private def runPreChecks(name      : Name,
                           invariants: Invariants[O, S, E],
                           arounds   : Arounds[O, S, E],
                           collapse  : Boolean,
                           p         : P): ActionResult.PreCheckFail[P] Or (PostCheckFn, (H, ROS, HH) => P) = {

    // Perform before
    val preResults: (PreCheck[H], PreCheck[UnpackChecks[List, O, S, E]]) = {
      val checksPreI = UnpackChecks.invariants(invariants, p.ros)
      val checksPreA = UnpackChecks.arounds(arounds, p.ros)
      val checksPre = PreCheck(checksPreA, checksPreI)
      val bI = History.newBuilder[E](stats)
      val bA = History.newBuilder[E](stats)
      checksPreA.errors foreach bA.addNE
      checksPreI.errors foreach bI.addNE
      bA.addEach(checksPreA.befores)(_.name)(p.ros.some, _.test(p.ros).noCause)
      bI.addEach(checksPreI.befores)(_.name)(p.ros.some, _.test(p.ros).noCause)
      val pre = PreCheck(bA.group(PreName), bI.group(PreName))
      (pre, checksPre)
    }

    val pre = preResults._1.pre ++ preResults._1.invariants
    val checksPre = preResults._2

    if (pre.failed) {
      val invariantsOk = !preResults._1.invariants.failed
      val p2 = p :+ History.parent(name, pre)
      Left(ActionResult.PreCheckFail(p2, invariantsOk))

    } else {

      def runHalfChecks(deltas: Iterable[Around.DeltaA[OS[O, S], E]],
                        prepend: Vector[HalfCheck[O, S, E]] = Vector.empty): Vector[HalfCheck[O, S, E]] = {
        var b = prepend
        for (d0 <- deltas) {
          val d = d0.aux
          val r = attempt.attempt(d.before(p.ros)).fold[Tri[FE, d.A]](Failed(_), _.noCause)
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

      val postAction: (H, ROS, HH) => P = {
        case (step, ros2, hh) =>
          val collapseIfNoPost = collapse && pre.isEmpty && step.steps.length == 1

          def collapsed = step.steps(0).copy(name = name)

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

      Right((postChecksFn, postAction))
    }
  }

  private def runPreChecksWithRetry[A, B](preCheckFn: P => ActionResult.PreCheckFail[A] Or B,
                                          reObserve : () => F[FE Or (R, O)],
                                          p0        : P): F[ActionResult.PreCheckFail[A] Or B] = {
    def retry: F[ActionResult.PreCheckFail[A] Or B] =
      EM.map(reObserve()) {
        case Right((r, o)) => preCheckFn(p0.copy(ros = ROS(r, o, p0.ros.state)))
        case Left(_) => preCheckFn(p0)
      }
    withRetryF1(Retry.Scope.PreConditions, EM.point(preCheckFn(p0)), retry)(retryCheckAround0)
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
      checksPostA = UnpackChecks.arounds(arounds, ros2)
      checksPostI = UnpackChecks.invariants(invariants, ros2)
    }

    // Post conditions
    val post = {
      val b = History.newBuilder[E](stats)
      checksPostA.errors foreach b.addNE
      b.addEach(hcs)(
        c => c.check.name)(Some(BeforeAfter(p.ros, ros2)),
        c => c.before.flatMap(a => Tri failedOption c.check.test(ros2, a).noCause)) // Perform around-post
      b.addEach(checksPostA.afters)(_.name)(ros2.some, _.test(ros2).noCause) // Perform post
      b.group(PostName)
    }

    // Check invariants
    val invs = {
      val b = History.newBuilder[E](stats)
      checksPostI.errors foreach b.addNE
      b.addEach(checksPostI.afters)(_.name)(ros2.some, _.test(ros2).noCause)
      b.group(InvariantsName)
    }

    PostCheckResults(post, invs)
  }

  private def runWithChecksAround(runPreChecksF: => F[ActionResult.PreCheckFail[P] Or (PostCheckFn, (H, ROS, HH) => P)],
                                  run          : PostCheckFn => F[(H, ROS, HH)]): F[P] =
    EM.flatMap(runPreChecksF) {
      case Right((postCheckFn, onComplete)) =>
        EM.map(run(postCheckFn))(onComplete.tupled)
      case Left(ActionResult.PreCheckFail(p2, _)) =>
        EM.pure(p2)
    }

  private def runAction[X](actionFn : ROS => Option[() => F[E Or (O => E Or S)]],
                           reObserve: () => F[FE Or (R, O)],
                           ros0     : ROS,
                           preChecks: ROS => ActionResult.PreCheckFail[P] Or X): F[ActionResult[O => E Or S, X]] = {

    type Result = ActionResult[O => E Or S, X]
    type ResultF = F[Result]

    def go(ros: ROS): ResultF =
      actionFn(ros) match {
        case Some(act1) =>

          preChecks(ros) match {
            case Right(x) =>
              stats.actions += 1
              def act2(): F[FE Or (O => E Or S)] = EM.map(act1())(_.leftMap(Failure NoCause _))
              def act3(): F[FE Or (O => E Or S)] = EM.recover(act2())
              EM.map(act3()) {
                case Right(nextStateFn) => ActionResult.Success(nextStateFn, x)
                case Left(e) => ActionResult.ActionFail(e, x)
              }

            case Left(e) => EM.pure(e)
          }

        case None =>
          EM.pure(ActionResult.Skip)
      }

    lazy val initialPreCheck = preChecks(ros0)

    def onRetry(initialFailure: Result): ResultF =
      EM.flatMap(reObserve()) {
        case Right((r, o)) => go(ROS(r, o, ros0.state))
        case Left(e)       =>
          val actionFailure: Option[FE] =
            initialFailure match {
              case ActionResult.ActionFail(fe, _) => Some(fe)
              case _ => None // The shit types don't let me prove this but this isn't possible
            }
          EM.pure(initialPreCheck.fold(identity, ActionResult.ObsFail(actionFailure, e, _)))
      }

    withRetryF1I(Retry.Scope.Action, go(ros0))(onRetry, _.retry)
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

              if (p.failed)
                EM.pure(p :+ History.Step.skip(nameFn))

              else {
                val name = attempt.name(nameFn, ros.some)

                val reObserve: () => F[FE Or (R, O)] =
                  () => EM.point {
                    val ref = refFn()
                    observeNoRetry(test, ref).map((ref, _))
                  }

                val invariants = nonInitialInvariants & test.invariants
                innerAction match {

                  // ==============================================================================
                  case Action.Single(prepareAction) =>

                    val actionResultF =
                      runAction(prepareAction, reObserve, p.ros, ros =>
                        runPreChecks(name, invariants, check, true, p.copy(ros = ros)))

                    def fail(f: (H, ROS, HH) => P, h: Name => H) =
                      f(h(ActionName), ros, PostCheckResults.empty)

                    EM.flatMap(actionResultF) {

                      case ActionResult.Success(nextStateFn, (postCheckFn, onComplete)) =>
                        EM.map(observeAndTestWithRetry(refFn, test, nextStateFn, postCheckFn(false))) {
                          case Right((ros2, hh)) => onComplete(histPassFn(ActionName), ros2, hh)
                          case Left((where, fe)) => fail(onComplete, hist2Fn(Pass, History.Step(where, Fail(fe))))
                        }

                      case ActionResult.PreCheckFail(p2, _) =>
                        EM.pure(p2)

                      case ActionResult.ActionFail(e, (_, onComplete)) =>
                        EM pure fail(onComplete, hist1Fn(Fail(e)))

                      case ActionResult.ObsFail(Some(ae), oe, (_, onComplete)) =>
                        EM pure fail(onComplete, hist2Fn(Fail(ae), History.Step(Observation, Fail(oe))))

                      case ActionResult.ObsFail(None, e, (_, onComplete)) =>
                        EM pure fail(onComplete, hist1Fn(Fail(e)))

                      case ActionResult.Skip =>
                        EM.pure(p :+ History.Step(name, Skip))
                    }

                  // ==============================================================================
                  case Action.Group(actions, cond) =>

                    def run(postCheckFn: PostCheckFn): F[(H, ROS, HH)] =
                      EM.flatMap(start(actions, ros))(childResults => {

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
                          _withRetryF(Retry.Scope.PostConditions, initialPostCheck)(reObserveAndPostCheck, _._2.retry)

                        EM.map(postCheckWithRetry)(x => (childResults.history, x._1, x._2))
                      })

                    // Retry here doesn't make sense. This bit is pure and deterministic
                    attempt.attempt(cond.permit(p.ros)) match {
                      case Right(Right(true)) =>
                        val preChecks1 = (p: P) => runPreChecks(name, invariants, check, false, p)
                        def preChecksF = runPreChecksWithRetry(preChecks1, reObserve, p)
                        runWithChecksAround(preChecksF, run)

                      case Right(Right(false)) =>
                        EM.pure(p :+ History.Step(name, Skip))

                      case Right(Left(e)) =>
                        EM.pure(p :+ History.Step(name, Fail(Failure NoCause e)))

                      case Left(e) =>
                        EM.pure(p :+ History.Step(name, Fail(e)))
                    }

                  // ==============================================================================
                  case Action.SubTest(action, subInvariants) =>
                    val runFn = (postChks: PostCheckFn) => {
                      val t    = Plan(action, subInvariants).test(test.observer)
                      val subP = subtest(t, invariants, refFn, ros, false)
                      EM.map(subP)(s => (s.history, s.ros, postChks(s.history.failed)(s.ros)))
                    }
                    val p0         = p.copy(history = History.empty)
                    val preChecks1 = (p: P) => runPreChecks(name, Sack.empty, check, false, p)
                    def preChecksF = runPreChecksWithRetry(preChecks1, reObserve, p0)
                    val logic      = runWithChecksAround(preChecksF, runFn)
                    EM.map(logic)(s => s.copy(history = p.history ++ s.history))
                }
            }

            case Left(NamedError(n, e)) =>
              EM.pure(p :+ History.Step(n, Fail(e)))
          }

        EM.map(processNextStep)(p => Progress.prepareNext(queue.tail, p.ros, p.history))
      }

    def runInvariants(ros: ROS): (ROS, H) = {
      val invariantsPoints = {
        val ps = new UniqueListBuilder[Point[OS[O, S], E]]
        val es = new UniqueListBuilder[NamedError[FE]]
        foreachSackE(test.invariants)(ros) {
          case Right(Invariant.Point(p)) => ps += p
          case Right(Invariant.Delta(_)) => ()
          case Left(e)                   => es += e
        }
        type T = CheckNE[Point, O, S, E]
        val pi = ps.iterator().map[T](Right(_))
        val ei = es.iterator().map[T](Left(_))
        (pi ++ ei).toList
      }

      val h: H =
        if (invariantsPoints.isEmpty)
          History.empty
        else {
          val children = {
            val b = History.newBuilder[E](stats)
            b.addEachNE(invariantsPoints)(_.name)(ros.some, _.test(ros).noCause)
            b.history()
          }
          History(History.parent(InitialState, children))
        }

      (ros, h)
    }

    def runInvariantsOnRetry(): FE Or (ROS, H) = {
      val r = refFn()
      observeNoRetry(test, r).map(o => runInvariants(ROS(r, o, initROS.state)))
    }

    def runAfterInvariants(ros: ROS, initialHistory: H): F[P] = {
      val fh: F[P] =
        if (initialHistory.failed)
          EM.pure(Progress[F, R, O, S, E](None, ros, initialHistory))
        else
          start(test.actions, ros, initialHistory)

      EM.map(fh) { p =>
        import p.{history => h}

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

        p.copy(history = h2)
      }
    }

    val runInvariantsWithRetry: F[FE Or (ROS, H)] =
      _withRetryF[FE Or (ROS, H)](
        Retry.Scope.InitialInvariants,
        Right(runInvariants(initROS)))(
        EM.point(runInvariantsOnRetry()), retryInvariants)

    EM.flatMap(runInvariantsWithRetry) {
      case Right((ros, h)) => runAfterInvariants(ros, h)
      case Left(e) =>
        val s = History.Step(Observation, Fail(e))
        val h = History(History.parent(InitialState, History(s)))
        EM.pure(Progress[F, R, O, S, E](None, initROS, h))
    }
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
      EM.map(observeWithRetry(test, ref)) {
        case Right(obs) =>
          attempt.attempt(nextStateFn(obs)) match {
            case Right(Right(state)) =>
              val ros = ROS(ref, obs, state)
              Right((ros, postChecks(ros)))
            case Right(Left(e)) => Left((UpdateState, Failure NoCause e))
            case Left(fe) => Left((UpdateState, fe))
          }
        case Left(e) => Left((Observation, e))
      }
    }

    withRetryF(Retry.Scope.PostAction, withoutRetry)(obsAndTestFailed)
  }
}
