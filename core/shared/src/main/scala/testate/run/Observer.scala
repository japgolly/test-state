package testate.run

import acyclic.file
import testate.data.{Or, Right}

final class Observer[-R, +O, +E](val observe: R => E Or O) extends AnyVal {
  @inline def apply(r: R) = observe(r)

  def cmapR[RR](f: RR => R): Observer[RR, O, E] =
    new Observer(observe compose f)

  def pmapR[RR, EE >: E](f: RR => EE Or R): Observer[RR, O, EE] =
    new Observer(r => f(r) flatMap observe)

  def mapO[OO](f: O => OO): Observer[R, OO, E] =
    new Observer(observe(_) map f)

  def mapE[EE](f: E => EE): Observer[R, O, EE] =
    new Observer(observe(_) leftMap f)
}

object Observer {

  def apply[R, O](f: R => O): Observer[R, O, Nothing] =
    new Observer(r => Right(f(r)))

  def attempt[R, O, E](f: R => E Or O): Observer[R, O, E] =
    new Observer(f)

  def watch[O](obs: => O): Observer[Any, O, Nothing] =
    new Observer(_ => Right(obs))

  def watchTry[O, E](obs: => E Or O): Observer[Any, O, E] =
    new Observer(_ => obs)

  def unit: Observer[Any, Unit, Nothing] =
    watch(())
}
