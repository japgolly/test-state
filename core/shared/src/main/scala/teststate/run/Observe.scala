package teststate.run

import acyclic.file
import teststate.data.Or

final case class Observe[-Ref, +Obs, +Err](val apply: Ref => Err Or Obs) extends AnyVal {
  def cmapR[R](f: R => Ref): Observe[R, Obs, Err] =
    Observe(apply compose f)

  def pmapR[R, E >: Err](f: R => E Or Ref): Observe[R, Obs, E] =
    Observe(r => f(r) flatMap apply)

  def mapO[O](f: Obs => O): Observe[Ref, O, Err] =
    Observe(apply(_) map f)

  def mapE[E](f: Err => E): Observe[Ref, Obs, E] =
    Observe(apply(_) leftMap f)
}

//  object Observe {
//    def Ops[R, O, E, Out](f: Observe[R, O, E] => Out): Ops[R, O, E, Out] =
//      new Ops[R, O, E, Out](f)
//
//    final class Ops[R, O, E, Out](private val observeOut: Observe[R, O, E] => Out) extends AnyVal {
//
//      def observe(f: R => O): Out =
//        observeTry(r => Right(f(r)))
//
//      def observeTry(f: R => E Or O): Out =
//        observeOut(Observe(f))
//
//    }
//  }
