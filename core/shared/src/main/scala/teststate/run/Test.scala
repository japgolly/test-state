package teststate.run

// import acyclic.file
import teststate.data._
import teststate.typeclass._
import teststate.core._
import CoreExports._
import CoreExports2._

sealed abstract class PlanLike[F[_], R, O, S, E, This] { self: This =>
  type Self[FF[_], RR, OO, SS, EE]

  // This causes scalac to produce stupid, spurious errors on the F type.
  // final type This = Self[F, R, O, S, E]

  protected def plan: Plan[F, R, O, S, E]
  protected def setPlan(plan: Plan[F, R, O, S, E]): This

  def name      : Option[Name]           = plan.name
  def actions   : Actions[F, R, O, S, E] = plan.actions
  def invariants: Invariants[O, S, E]    = plan.invariants
  implicit val executionModel: ExecutionModel[F] = plan.executionModel

//  def trans[G[_]: ExecutionModel](t: F ~~> G): Self[G, R, O, S, E]
//  def mapR[R2](f: R2 => R): Self[F, R2, O, S, E]
//  def pmapR[R2](f: R2 => E Or R): Self[F, R2, O, S, E]
//  def pmapO[OO](g: OO => E Or O): Self[F, R, OO, S, E]
//  def mapS[SS](g: SS => S)(s: (SS, S) => SS): Self[F, R, O, SS, E]
//  def mapE[EE](f: E => EE): Self[F, R, O, S, EE]
//  def lift[F2[_], R2, O2, S2, E2](implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Self[F2, R2, O2, S2, E2]

  final protected def modPlan(f: Plan[F, R, O, S, E] => Plan[F, R, O, S, E]): This =
    setPlan(f(plan))

  final def setActions(actions: Actions[F, R, O, S, E]): This =
    modPlan(p => new Plan(p.name, actions, p.invariants))

  final def setInvariants(invariants: Invariants[O, S, E]): This =
    modPlan(p => new Plan(p.name, p.actions, invariants))

  final def clearName: This =
    modPlan(p => new Plan(None, p.actions, p.invariants))

  final def named(name: Name): This =
    modPlan(p => new Plan(Some(name), p.actions, p.invariants))

  final def modActions(f: Actions[F, R, O, S, E] => Actions[F, R, O, S, E]): This =
    setActions(f(actions))

  final def modInvariants(f: Invariants[O, S, E] => Invariants[O, S, E]): This =
    setInvariants(f(invariants))

  def addInvariants(i: Invariants[O, S, E]): This =
    modInvariants(_ & i)

  // TODO asAction that uses this.name
  final def asAction(name: NameFn[ROS[R, O, S]]): Actions[F, R, O, S, E] =
    Action.liftInner(Action.SubTest(actions, invariants))(name)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object Plan {
  def apply[F[_], R, O, S, E](a: Actions[F, R, O, S, E], i: Invariants[O, S, E])(implicit em: ExecutionModel[F]): Plan[F, R, O, S, E] =
    new Plan(None, a, i)(em)

  def empty[F[_], R, O, S, E](implicit em: ExecutionModel[F]): Plan[F, R, O, S, E] =
    apply[F, R, O, S, E](emptyAction, emptyInvariants)

  def action[F[_], R, O, S, E](a: Actions[F, R, O, S, E])(implicit em: ExecutionModel[F]): Plan[F, R, O, S, E] =
    apply(a, emptyInvariants)(em)

  def invariants[F[_], R, O, S, E](i: Invariants[O, S, E])(implicit em: ExecutionModel[F]): Plan[F, R, O, S, E] =
    apply[F, R, O, S, E](emptyAction, i)(em)

  implicit def planInstanceShow[F[_], R, O, S, E](implicit sa: Show[Actions[F, R, O, S, E]],
                                                           si: Show[Invariants[O, S, E]]): Show[Plan[F, R, O, S, E]] =
    Show(p =>
      s"""
         |Invariants:
         |${si.indent(p.invariants)}
         |Actions:
         |${sa.indent(p.actions)}
       """.stripMargin.trim
    )
}

final class Plan[F[_], R, O, S, E](override val name: Option[Name],
                                   override val actions: Actions[F, R, O, S, E],
                                   override val invariants: Invariants[O, S, E])
                                  (implicit override val executionModel: ExecutionModel[F])
    extends PlanLike[F, R, O, S, E, Plan[F, R, O, S, E]] {

  override def toString: String =
    s"Plan($name, $actions, $invariants)"

  override type Self[FF[_], RR, OO, SS, EE] = Plan[FF, RR, OO, SS, EE]

  override protected def plan                            = this
  override protected def setPlan(p: Plan[F, R, O, S, E]) = p

  def trans[G[_]: ExecutionModel](t: F ~~> G): Self[G, R, O, S, E] =
    new Plan(name, actions trans t, invariants)

  def mapR[R2](f: R2 => R): Self[F, R2, O, S, E] =
    new Plan(name, actions mapR f, invariants)

  def pmapR[R2](f: R2 => E Or R): Self[F, R2, O, S, E] =
    new Plan(name, actions pmapR f, invariants)

  def pmapO[OO](g: OO => E Or O): Self[F, R, OO, S, E] =
    new Plan(name, actions pmapO g, invariants pmapO g)

  def mapS[SS](g: SS => S)(s: (SS, S) => SS): Self[F, R, O, SS, E] =
    new Plan(name, actions.mapS(g)(s), invariants.mapS(g))

  def mapE[EE](f: E => EE): Self[F, R, O, S, EE] =
    new Plan(name, actions mapE f, invariants mapE f)

  def lift[F2[_], R2, O2, S2, E2](implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Self[F2, R2, O2, S2, E2] =
    new Plan(name, t action actions, t invariant invariants)(t.f2)

  def withInitialState(s: S) =
    PlanWithInitialState(this, s)

  def stateless(implicit ev: Unit =:= S) =
    withInitialState(())

  def test(observer: Observer[R, O, E])(implicit r: Recover[E]) =
    Test(this, observer)(r)

  def testU(implicit ev: Observer[R, Unit, E] =:= Observer[R, O, E], r: Recover[E]) =
    test(ev(Observer.unit))(r)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final case class PlanWithInitialState[F[_], R, O, S, E](override val plan: Plan[F, R, O, S, E], initialState: S)
    extends PlanLike[F, R, O, S, E, PlanWithInitialState[F, R, O, S, E]] {

  override type Self[FF[_], RR, OO, SS, EE] = PlanWithInitialState[FF, RR, OO, SS, EE]

  override protected def setPlan(p: Plan[F, R, O, S, E]) = copy(plan = p)

  def trans[G[_]: ExecutionModel](t: F ~~> G): Self[G, R, O, S, E] =
    plan.trans(t).withInitialState(initialState)

  def mapR[R2](f: R2 => R): Self[F, R2, O, S, E] =
    plan.mapR(f).withInitialState(initialState)

  def pmapR[R2](f: R2 => E Or R): Self[F, R2, O, S, E] =
    plan.pmapR(f).withInitialState(initialState)

  def pmapO[OO](g: OO => E Or O): Self[F, R, OO, S, E] =
    plan.pmapO(g).withInitialState(initialState)

//  def mapS[SS](g: SS => S)(s: (SS, S) => SS): Self[F, R, O, SS, E] =
//  def mapS[SS](f: S => SS)(g: SS => S): Self[F, R, O, SS, E] =
//    plan.mapS(g)((_, s) => f(s)).withInitialState(f(initialState))

  def mapE[EE](f: E => EE): Self[F, R, O, S, EE] =
    plan.mapE(f).withInitialState(initialState)

//  def lift[F2[_], R2, O2, S2, E2](implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Self[F2, R2, O2, S2, E2] =
//    plan.lift(t).withInitialState(initialState)

  def test(observer: Observer[R, O, E])(implicit r: Recover[E]) =
    TestWithInitialState(plan.test(observer)(r), initialState)

  def testU(implicit ev: Observer[R, Unit, E] =:= Observer[R, O, E], r: Recover[E]) =
    test(ev(Observer.unit))(r)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final case class Test[F[_], R, O, S, E](override val plan: Plan[F, R, O, S, E], observer: Observer[R, O, E])
                                       (implicit val recover: Recover[E])
    extends PlanLike[F, R, O, S, E, Test[F, R, O, S, E]] {

  override type Self[FF[_], RR, OO, SS, EE] = Test[FF, RR, OO, SS, EE]

  override def setPlan(p: Plan[F, R, O, S, E]) = copy(plan = p)

  def trans[G[_]: ExecutionModel](t: F ~~> G): Self[G, R, O, S, E] =
    copy(plan = plan trans t)

  def mapR[R2](f: R2 => R): Self[F, R2, O, S, E] =
    Test(plan mapR f, observer cmapR f)

  def pmapR[R2](f: R2 => E Or R): Self[F, R2, O, S, E] =
    Test(plan pmapR f, observer pmapR f)

//  def pmapO[OO](g: OO => E Or O): Self[F, R, OO, S, E] =
//    Test(plan pmapO g, observer pmapO g)

  def mapS[SS](g: SS => S)(s: (SS, S) => SS): Self[F, R, O, SS, E] =
    copy(plan = plan.mapS(g)(s))

  def mapE[EE](f: E => EE): Self[F, R, O, S, EE] =
    Test(plan mapE f, observer mapE f)(recover map f)

//  def lift[F2[_], R2, O2, S2, E2](implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Self[F2, R2, O2, S2, E2] =
//    Test(plan.lift(t), observer)(recover)

  def withInitialState(s: S) =
    TestWithInitialState(this, s)

  def stateless(implicit ev: Unit =:= S) =
    withInitialState(())

  def run(initialState: S, ref: => R): F[Report[E]] =
    Runner.run(this)(initialState, ref)

  def runU(initialState: S)(implicit ev: Unit =:= R): F[Report[E]] =
    run(initialState, ())
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final case class TestWithInitialState[F[_], R, O, S, E](test: Test[F, R, O, S, E], initialState: S)
  extends PlanLike[F, R, O, S, E, TestWithInitialState[F, R, O, S, E]] {

  override type Self[FF[_], RR, OO, SS, EE] = TestWithInitialState[FF, RR, OO, SS, EE]

  override def plan                            = test.plan
  override def setPlan(p: Plan[F, R, O, S, E]) = test.setPlan(p).withInitialState(initialState)

  def recover  = test.recover
  def observer = test.observer

  def trans[G[_]: ExecutionModel](t: F ~~> G): Self[G, R, O, S, E] =
    test.trans(t).withInitialState(initialState)

  def mapR[R2](f: R2 => R): Self[F, R2, O, S, E] =
    test.mapR(f).withInitialState(initialState)

  def pmapR[R2](f: R2 => E Or R): Self[F, R2, O, S, E] =
    test.pmapR(f).withInitialState(initialState)

//  def pmapO[OO](g: OO => E Or O): Self[F, R, OO, S, E] =
//    test.pmapO(g).withInitialState(initialState)

//  def mapS[SS](g: SS => S)(s: (SS, S) => SS): Self[F, R, O, SS, E] =
//    test.mapS(g)((_, s) => f(s)).withInitialState(f(initialState))

  def mapE[EE](f: E => EE): Self[F, R, O, S, EE] =
    test.mapE(f).withInitialState(initialState)

//  def lift[F2[_], R2, O2, S2, E2](implicit t: Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]): Self[F2, R2, O2, S2, E2] =
//    test.lift(t).withInitialState(initialState)

  def planWithInitialState =
    plan.withInitialState(initialState)

  def run(ref: => R): F[Report[E]] =
    Runner.run(test)(initialState, ref)

  def runU(implicit ev: Unit =:= R): F[Report[E]] =
    run(())
}
