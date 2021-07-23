package teststate.core

import japgolly.microlibs.name_fn._
import teststate.core.Types._
import teststate.data._
import teststate.typeclass.Conditional.Implicits._
import teststate.typeclass._

object Action {
  sealed abstract class Inner[F[_], R, O, S, E]

  type ActionResult[F[_], O, S, E] = F[E Or (O => E Or S)]
  type Prepared[F[_], O, S, E] = Option[() => ActionResult[F, O, S, E]]

  final case class Single[F[_], R, O, S, E](run: ROS[R, O, S] => Prepared[F, O, S, E]) extends Inner[F, R, O, S, E] {
    def modPoly[F2[_], R2, O2, S2, E2](f: (ROS[R, O, S] => Prepared[F, O, S, E]) => ROS[R2, O2, S2] => Prepared[F2, O2, S2, E2]): Single[F2, R2, O2, S2, E2] =
      Single(f(run))

    @inline def mod(f: (ROS[R, O, S] => Prepared[F, O, S, E]) => ROS[R, O, S] => Prepared[F, O, S, E]): Single[F, R, O, S, E] =
      modPoly(f)

    def modAction(f: (ROS[R, O, S], () => ActionResult[F, O, S, E]) => ActionResult[F, O, S, E]): Single[F, R, O, S, E] =
      mod(run =>
        ros =>
          run(ros).map(actionFn => () => f(ros, actionFn)))
  }
  object Single {
    def empty[F[_], R, O, S, E](implicit F: ExecutionModel[F]): Single[F, R, O, S, E] =
      apply(ros => Some(() => F.pure(Right((_: O) => Right(ros.state)))))
    def skip[F[_], R, O, S, E]: Single[F, R, O, S, E] =
      apply(_ => None)
  }

  final case class Group[F[_], R, O, S, E](actions: Actions[F, R, O, S, E],
                                           cond: Group.Cond[R, O, S, E]) extends Inner[F, R, O, S, E]

  object Group {

    def lift[F[_], R, O, S, E](actions: Actions[F, R, O, S, E]): Group[F, R, O, S, E] =
      apply[F, R, O, S, E](actions, Cond.always)

    final case class Cond[-R, -O, -S, +E](permit: ROS[R, O, S] => E Or Boolean) extends AnyVal { self =>
      def skip(ros: ROS[R, O, S]): E Or Boolean =
        permit(ros).map(!_)

      def |+|[RR <: R, OO <: O, SS <: S, EE >: E](next: Cond[RR, OO, SS, EE]): Cond[RR, OO, SS, EE] =
        if (permit eq Cond.always.permit)
          next
        else
          Cond[RR, OO, SS, EE](ros =>
            for {
              a <- self.permit(ros)
              b <- next.permit(ros)
            } yield a && b
          )

      def mapE[A](f: E => A): Cond[R, O, S, A] =
        Cond(permit(_).leftMap(f))

      def mapR[A](f: A => R): Cond[A, O, S, E] =
        Cond(ros => permit(ros.mapR(f)))

      def pmapR[A, EE >: E](f: A => EE Or R): Cond[A, O, S, EE] =
        Cond(ros => ros.emapR(f).flatMap(permit))

      def pmapO[A, EE >: E](f: A => EE Or O): Cond[R, A, S, EE] =
        Cond(ros => ros.emapO(f).flatMap(permit))

      def mapOS[A, B](f: A => O, g: B => S): Cond[R, A, B, E] =
        Cond(ros => permit(ros.mapOS(f, g)))

      def modS[OO <: O, SS <: S, EE >: E](m: OO => SS => EE Or SS): Cond[R, OO, SS, EE] =
        Cond(ros => m(ros.obs)(ros.state).flatMap(s2 => permit(ros.withState(s2))))
    }

    object Cond {
      val always = Cond[Any, Any, Any, Nothing](_ => Or.rightTrue)

      def when[R, O, S](f: ROS[R, O, S] => Boolean): Cond[R, O, S, Nothing] =
        apply(f.andThen(Right(_)))

      def unless[R, O, S](f: ROS[R, O, S] => Boolean): Cond[R, O, S, Nothing] =
        apply(f.andThen(b => Right(!b)))
    }
  }

  final case class SubTest[F[_], R, O, S, E](action    : Actions[F, R, O, S, E],
                                             invariants: Invariants[O, S, E]) extends Inner[F, R, O, S, E]

  final case class Outer[F[_], R, O, S, E](name : NameFn[ROS[R, O, S]],
                                           inner: Inner[F, R, O, S, E],
                                           check: Arounds[O, S, E])

  object Outer {
    def skip[F[_], R, O, S, E](name: Name): Outer[F, R, O, S, E] =
      apply(NameFn.const(name), Single.skip, Sack.empty)
  }

  type Actions[F[_], R, O, S, E] = SackE[ROS[R, O, S], Outer[F, R, O, S, E], E]

  def empty[F[_], R, O, S, E]: Actions[F, R, O, S, E] =
    Sack.empty

  def liftOuter[F[_], R, O, S, E](outer: Outer[F, R, O, S, E]): Actions[F, R, O, S, E] =
    Sack Value Right(outer)

  def liftInner[F[_], R, O, S, E](inner: Inner[F, R, O, S, E])(name: NameFn[ROS[R, O, S]]): Actions[F, R, O, S, E] =
    liftOuter(Outer(name, inner, Sack.empty))

  implicit def actionOuterInstanceNamedOps[F[_], R, O, S, E]: NamedOps[Outer[F, R, O, S, E], ROS[R, O, S]] =
    NamedOps((a, f) => a.copy(name = f(a.name)))

  implicit def actionOuterInstanceDisplay[F[_], R, O, S, E]: Display[Outer[F, R, O, S, E]] =
    Display(_.name(None).value)

  implicit def actionInnerInstanceConditional[F[_], R, O, S, E]: Conditional[Inner[F, R, O, S, E], ROS[R, O, S]] =
    Conditional((m, f) => m match {
      case a: Single [F, R, O, S, E] => a.copy(run = a.run when f)
      case a: Group  [F, R, O, S, E] => a.copy(cond = a.cond |+| Group.Cond.when(f))
      case a: SubTest[F, R, O, S, E] => a.copy(action = a.action when f)
    })

  implicit def actionOuterInstanceConditional[F[_], R, O, S, E]: Conditional[Outer[F, R, O, S, E], ROS[R, O, S]] =
    Conditional((a, f) => a.copy(inner = a.inner when f))

  // Scala needs this since partial unification
  implicit def actionsProfunctor[F[_], R, O, S, E](a: Actions[F, R, O, S, E]) =
    new Profunctor.Ops[Sack, ROS[R, O, S], NamedError[Failure[E]] Or Action.Outer[F, R, O, S, E]](a)
}
