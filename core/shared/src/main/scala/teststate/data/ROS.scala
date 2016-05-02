package teststate.data

import acyclic.file

// TODO Fix this crap

final case class ROS[+R, +O, +S](ref: R, obs: O, state: S) {

  val some: Some[this.type] =
    Some(this)

  val os: OS[O, S] =
    OS(obs, state)

  val sos: Some[OS[O, S]] =
    Some(os)

  def mapR  [X]   (f: R => X)           : ROS[X, O, S]      = copy(ref = f(ref))
  def mapO  [X]   (f: O => X)           : ROS[R, X, S]      = copy(obs = f(obs))
  def mapS  [X]   (f: S => X)           : ROS[R, O, X]      = copy(state = f(state))
  def mapOS [X, Y](o: O => X, s: S => Y): ROS[R, X, Y]      = ROS(ref, o(obs), s(state))
  def emapR [E, X](f: R => E Or X)      : E Or ROS[X, O, S] = f(ref).map(ROS(_, obs, state))
  def emapO [E, X](f: O => E Or X)      : E Or ROS[R, X, S] = f(obs).map(ROS(ref, _, state))
}


final case class OS[+O, +S](obs: O, state: S) {
  def map  [OO, SS](o: O => OO, s: S => SS): OS[OO, SS]     = OS(o(obs), s(state))
  def mapO [A]     (f: O => A)             : OS[A, S]       = OS(f(obs), state)
  def mapS [SS]    (f: S => SS)            : OS[O, SS]      = OS(obs, f(state))
  def emapO[E, OO] (f: O => E Or OO)       : E Or OS[OO, S] = f(obs).map(OS(_, state))
}

