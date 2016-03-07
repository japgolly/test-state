package teststate.data

import acyclic.file

// TODO Fix this crap

final class ROS[+Ref, +Obs, +State](refFn: () => Ref, val obs: Obs, val state: State) {
  val some: Some[this.type] =
    Some(this)

  val os: OS[Obs, State] =
    OS(obs, state)

  val rOS: Right[OS[Obs, State]] =
    Right(os)

  val sos: Some[OS[Obs, State]] =
    Some(os)

  lazy val ref = refFn()

  def setRef[R](ref: => R) = new ROS[R, Obs, State](() => ref, obs, state)
  def copyOS[OO, SS](obs: OO = obs, state: SS = state) = new ROS[Ref, OO, SS](() => ref, obs, state)

  def mapR[A](f: Ref => A) = new ROS(() => f(ref), obs, state)

  def mapO[A](f: Obs   => A): ROS[Ref, A, State] = copyOS(obs = f(obs))
  def mapS[A](f: State => A): ROS[Ref, Obs, A] = copyOS(state = f(state))
  def mapOS[OO, SS](o: Obs => OO, s: State => SS): ROS[Ref, OO, SS] = copyOS(o(obs), s(state))
  def mapOe[OO](f: Obs => Any Or OO): Option[ROS[Ref, OO, State]] = f(obs).toOptionMap(o => copyOS(obs = o))
  def mapRe[RR](f: Ref => Any Or RR): Option[ROS[RR, Obs, State]] = f(ref).toOptionMap(setRef(_))
}


final case class OS[+O, +S](obs: O, state: S) {
  def map[OO, SS](o: O => OO, s: S => SS) = OS(o(obs), s(state))
  def mapO[A](f: O => A) = OS(f(obs), state)
//    def mapOo[A](f: O => Option[A]) = f(obs).map(OS(_, state))
  def mapOe[OO](f: O => Any Or OO): Option[OS[OO, S]] = f(obs).toOptionMap(OS(_, state))
  def mapOE[E, OO](f: O => E Or OO): E Or OS[OO, S] = f(obs).map(OS(_, state))
  def mapS[SS](f: S => SS) = OS(obs, f(state))
}

