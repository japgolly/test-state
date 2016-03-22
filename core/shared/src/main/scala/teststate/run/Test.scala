package teststate.run

// import acyclic.file
import teststate.data._
import teststate.typeclass._
import teststate.core._
import CoreExports._
import CoreExports2._

// TODO Maybe better: Script | Plan | TestCase
class TestContent[F[_], R, O, S, E](val action: Actions[F, R, O, S, E],
                                    val invariants: Invariants[O, S, E])
                                   (implicit val executionModel: ExecutionModel[F], val recover: Recover[E]) {
  def trans[G[_]: ExecutionModel](t: F ~~> G): TestContent[G, R, O, S, E] =
    new TestContent(action trans t, invariants)

  def mapR[R2](f: R2 => R): TestContent[F, R2, O, S, E] =
    new TestContent(action mapR f, invariants)

  def pmapR[R2](f: R2 => E Or R): TestContent[F, R2, O, S, E] =
    new TestContent(action pmapR f, invariants)

  def pmapO[OO](g: OO => E Or O): TestContent[F, R, OO, S, E] =
    new TestContent[F, R, OO, S, E](
      action.pmapO(g),
      invariants.pmapO(g))

  def mapS[SS](s: SS => S, su: (SS, S) => SS): TestContent[F, R, O, SS, E] =
    new TestContent(
      action.mapS(s)(su),
      invariants.mapS(s))

  def mapE[EE](f: E => EE): TestContent[F, R, O, S, EE] =
    new TestContent(
      action mapE f,
      invariants mapE f)(executionModel, recover map f)

  def lift[F2[_], R2, O2, S2, E2](implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2], r: Recover[E2]): TestContent[F2, R2, O2, S2, E2] =
    new TestContent(t action action, t invariant invariants)(t.f2, r)

  def addInvariants(i: Invariants[O, S, E]): TestContent[F, R, O, S, E] =
    new TestContent(action, invariants & i)

  def asAction(name: NameFn[ROS[R, O, S]]): Actions[F, R, O, S, E] =
    Action.liftInner(Action.SubTest(action, invariants))(name)

  def observe(f: R => O) =
    observeTry(r => Right(f(r)))

  def observeTry(f: R => E Or O) =
    new Test(this, Observe(f))
}

object TestContent {
  implicit def testContentInstanceShow[F[_], R, O, S, E](implicit sa: Show[Actions[F, R, O, S, E]],
                                                         si: Show[Invariants[O, S, E]]): Show[TestContent[F, R, O, S, E]] =
    Show(tc =>
      s"""
         |Invariants:
         |${si.indent(tc.invariants)}
         |Actions:
         |${sa.indent(tc.action)}
       """.stripMargin.trim
    )
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

  def addInvariants(i: Invariants[Obs, State, Err]): Test[F, Ref, Obs, State, Err] =
    new Test(content addInvariants i, observe)

  def run(initialState: State, ref: => Ref): F[Report[Err]] =
    Runner.run(this)(initialState, ref)
}

object Test {
  def apply[F[_], Ref, Obs, State, Err](action: Actions[F, Ref, Obs, State, Err],
                                        invariants: Invariants[Obs, State, Err] = Sack.empty)
                                       (implicit em: ExecutionModel[F], recover: Recover[Err]) =
    new TestContent(action, invariants)(em, recover)

  implicit def testInstanceShow[F[_], R, O, S, E](implicit s: Show[TestContent[F, R, O, S, E]]): Show[Test[F, R, O, S, E]] =
    s.cmap(_.content)
}
