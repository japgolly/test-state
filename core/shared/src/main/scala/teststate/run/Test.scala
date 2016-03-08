package teststate.run

// import acyclic.file
import teststate.data._
import teststate.typeclass._
import teststate.core._
import CoreExports._

// TODO Maybe better: Script | Plan | TestCase
class TestContent[F[_], Ref, Obs, State, Err](val action: Action[F, Ref, Obs, State, Err],
                                              val invariants: Invariants[Obs, State, Err])
                                             (implicit val executionModel: ExecutionModel[F], val recover: Recover[Err]) {
  def trans[G[_]: ExecutionModel](t: F ~~> G): TestContent[G, Ref, Obs, State, Err] =
    new TestContent(action trans t, invariants)

  def mapR[R2](f: R2 => Ref): TestContent[F, R2, Obs, State, Err] =
    new TestContent(action cmapR f, invariants)

  def pmapR[R2](f: R2 => Err Or Ref): TestContent[F, R2, Obs, State, Err] =
    new TestContent(action pmapR f, invariants)

  def pmapO[OO](g: OO => Err Or Obs): TestContent[F, Ref, OO, State, Err] =
    new TestContent[F, Ref, OO, State, Err](
      action.pmapO(g),
      invariants.pmapO(g))

  def mapS[SS](s: SS => State, su: (SS, State) => SS): TestContent[F, Ref, Obs, SS, Err] =
    new TestContent(
      action.unzoomS(s, su),
      invariants.mapS(s))

  def mapE[E](f: Err => E): TestContent[F, Ref, Obs, State, E] =
    new TestContent(
      action mapE f,
      invariants mapE f)(executionModel, recover map f)

  def addInvariants(i: Invariants[Obs, State, Err]): TestContent[F, Ref, Obs, State, Err] =
    new TestContent(action, invariants & i)

  def asAction(name: NameFn[ROS[Ref, Obs, State]]) =
    Action.SubTest(name, action, invariants)

  def observe(f: Ref => Obs) =
    observeTry(r => Right(f(r)))

  def observeTry(f: Ref => Err Or Obs) =
    new Test(this, Observe(f))
}

class Test[F[_], Ref, Obs, State, Err](val content: TestContent[F, Ref, Obs, State, Err],
                                       val observe: Observe[Ref, Obs, Err]) {
  def trans[G[_]: ExecutionModel](t: F ~~> G): Test[G, Ref, Obs, State, Err] =
    new Test(content trans t, observe)

  def mapR[R2](f: R2 => Ref): Test[F, R2, Obs, State, Err] =
    new Test(content mapR f, observe cmapR f)

  def pmapR[R2](f: R2 => Err Or Ref): Test[F, R2, Obs, State, Err] =
    new Test(content pmapR f, observe pmapR f)

  def mapS[SS](s: SS => State, su: (SS, State) => SS): Test[F, Ref, Obs, SS, Err] =
    new Test(content.mapS(s, su), observe)

  def mapE[E](f: Err => E): Test[F, Ref, Obs, State, E] =
    new Test(content mapE f, observe mapE f)

  def run(initialState: State, ref: => Ref): F[History[Err]] =
    Runner.run(this)(initialState, ref)

  def addInvariants(i: Invariants[Obs, State, Err]): Test[F, Ref, Obs, State, Err] =
    new Test(content addInvariants i, observe)
}

object Test {
  def apply[F[_], Ref, Obs, State, Err](action: Action[F, Ref, Obs, State, Err],
                                        invariants: Invariants[Obs, State, Err] = Sack.empty)
                                       (implicit em: ExecutionModel[F], recover: Recover[Err]) =
    new TestContent(action, invariants)(em, recover)
}
