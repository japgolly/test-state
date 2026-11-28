package teststate.run

import teststate.data.{Left, Or, Right}
import teststate.typeclass.{ExecutionModel, ~~>}

final case class Observer[F[_], R, O, E](observe: R => F[E Or O])(implicit F: ExecutionModel[F]) {
  @inline def apply(r: R) = observe(r)

  def trans[G[_]: ExecutionModel](f: F ~~> G): Observer[G, R, O, E] =
    new Observer(r => f(observe(r)))

  def cmapR[RR](f: RR => R): Observer[F, RR, O, E] =
    new Observer(observe compose f)

  def pmapR[RR](f: RR => E Or R): Observer[F, RR, O, E] =
    new Observer(rr => f(rr) match {
      case Right(r) => observe(r)
      case Left(ee) => F.pure(Left(ee))
    })

  def mapO[OO](f: O => OO): Observer[F, R, OO, E] =
    new Observer(r => F.map(observe(r))(_ map f))

  def mapE[EE](f: E => EE): Observer[F, R, O, EE] =
    new Observer(r => F.map(observe(r))(_ leftMap f))
}

object Observer {
  def unit[F[_], R, E](implicit F: ExecutionModel[F]): Observer[F, R, Unit, E] = {
    val f = F.pure[E Or Unit](Right(()))
    new Observer(_ => f)
  }
}
