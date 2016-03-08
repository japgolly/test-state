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

  def mapR  [X]   (f: R => X)           : ROS[X, O, S]         = copy(ref = f(ref))
  def mapO  [X]   (f: O => X)           : ROS[R, X, S]         = copy(obs = f(obs))
  def mapS  [X]   (f: S => X)           : ROS[R, O, X]         = copy(state = f(state))
  def mapOS [X, Y](o: O => X, s: S => Y): ROS[R, X, Y]         = ROS(ref, o(obs), s(state))
  def mapOe [X]   (f: O => Any Or X)    : Option[ROS[R, X, S]] = f(obs).toOptionMap(o => copy(obs = o))
  def mapRe [X]   (f: R => Any Or X)    : Option[ROS[X, O, S]] = f(ref).toOptionMap(r => copy(ref = r))
}


final case class OS[+O, +S](obs: O, state: S) {
  def map[OO, SS](o: O => OO, s: S => SS) = OS(o(obs), s(state))
  def mapO[A](f: O => A) = OS(f(obs), state)
//    def mapOo[A](f: O => Option[A]) = f(obs).map(OS(_, state))
  def mapOe[OO](f: O => Any Or OO): Option[OS[OO, S]] = f(obs).toOptionMap(OS(_, state))
  def mapOE[E, OO](f: O => E Or OO): E Or OS[OO, S] = f(obs).map(OS(_, state))
  def mapS[SS](f: S => SS) = OS(obs, f(state))
}

