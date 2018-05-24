package teststate.data

import acyclic.file

sealed class OS[+O, +S](val obs: O, val state: S) {
  override def hashCode(): Int =
    obs.## * 31 + state.##

  override def equals(obj: Any): Boolean =
    obj match {
      case _: ROS[Any, Any, Any] => false
      case that: OS[Any, Any] => this.obs == that.obs && this.state == that.state
      case _ => false
    }

  def withObs[A](newObs: A)    : OS[A, S] = new OS(newObs, state)
  def withState[A](newState: A): OS[O, A] = new OS(obs, newState)

  def mapOS[X, Y](o: O => X, s: S => Y): OS[X, Y]      = new OS(o(obs), s(state))
  def mapO [X]   (f: O => X)           : OS[X, S]      = new OS(f(obs), state)
  def mapS [X]   (f: S => X)           : OS[O, X]      = new OS(obs, f(state))
  def emapO[E, X](f: O => E Or X)      : E Or OS[X, S] = f(obs).map(new OS(_, state))
}

object OS {
  def apply[O, S](obs: O, state: S): OS[O, S] =
    new OS(obs, state)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final class ROS[+R, +O, +S](val ref: R, override val obs: O, override val state: S) extends OS[O, S](obs, state) {
  override def hashCode(): Int =
    ref.## + super.hashCode * 31

  override def equals(obj: Any): Boolean =
    obj match {
      case that: ROS[Any, Any, Any] => this.ref == that.ref && this.obs == that.obs && this.state == that.state
      case _ => false
    }

           def withRef[A](newRef: A)    : ROS[A, O, S] = new ROS(newRef, obs, state)
  override def withObs[A](newObs: A)    : ROS[R, A, S] = new ROS(ref, newObs, state)
  override def withState[A](newState: A): ROS[R, O, A] = new ROS(ref, obs, newState)

  val some: Some[this.type] =
    Some(this)

  def mapR [X]   (f: R => X)     : ROS[X, O, S]      = new ROS(f(ref), obs, state)
  def emapR[E, X](f: R => E Or X): E Or ROS[X, O, S] = f(ref).map(new ROS(_, obs, state))

  override def mapO [X]   (f: O => X)           : ROS[R, X, S]      = new ROS(ref, f(obs), state)
  override def mapS [X]   (f: S => X)           : ROS[R, O, X]      = new ROS(ref, obs, f(state))
  override def mapOS[X, Y](o: O => X, s: S => Y): ROS[R, X, Y]      = new ROS(ref, o(obs), s(state))
  override def emapO[E, X](f: O => E Or X)      : E Or ROS[R, X, S] = f(obs).map(new ROS(ref, _, state))
}

object ROS {
  def apply[R, O, S](ref: R, obs: O, state: S): ROS[R, O, S] =
    new ROS(ref, obs, state)
}
