package teststate.core

import teststate.core.Around.{After, Before, When}
import teststate.core.Types._
import teststate.data.Sack

object PointOps {

  final class Ops[O, S, E](private val p: Points[O, S, E]) extends AnyVal {
    private def toAround(when: When): Arounds[O, S, E] =
      p.rmap(_ map (Around.Point(_, when)))

    def before: Arounds[O, S, E] =
      toAround(Before)

    def after: Arounds[O, S, E] =
      toAround(After)

    def beforeAndAfter: Arounds[O, S, E] =
      Sack.append(before, after)
  }

  trait Implicits {
    implicit def pointsToPointsOps[O, S, E](p: Points[O, S, E]): Ops[O, S, E] =
      new Ops(p)
  }
}
