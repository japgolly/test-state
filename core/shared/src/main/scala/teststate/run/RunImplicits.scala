package teststate.run

import acyclic.file
import teststate.data.Name
import teststate.core.CoreExports._
import teststate.core.CoreExports3._
import teststate.typeclass.PolyComposable
import PolyComposable._

trait RunImplicits {

  implicit def planComposable[F[_], R, O, S, E]: Mono[SeqOp, Plan[F, R, O, S, E]] =
    PolyComposable { (x, y) =>
      val n: Option[Name] =
        (x.name, y.name) match {
          case (None, None)       => None
          case (n@Some(_), None)  => n
          case (None, n@Some(_))  => n
          case (Some(a), Some(b)) => Some(s"${a.value} >> ${b.value}")
        }
      val as = x.actions >> y.actions
      val is = x.invariants & y.invariants
      new Plan(n, as, is)(x.executionModel)
    }

  implicit def plansCanSeq[F[_], R, O, S, E]: Can[SeqOp, Plan[F, R, O, S, E]] = Can
}
