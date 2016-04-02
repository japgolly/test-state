package testate

import acyclic.file
import nyaya.gen._
import nyaya.test.Settings
import testate.data.ROS
import Exports._
import TestStateNyaya._

trait TestStateNyaya {
  implicit def toDslNyayaOps[F[_], R, O, S, E](dsl: Dsl[F, R, O, S, E]): DslNyayaOps[F, R, O, S, E] =
    new DslNyayaOps(dsl)
}

object TestStateNyaya extends TestStateNyaya {

  final class DslNyayaOps[F[_], R, O, S, E](private val dsl: Dsl[F, R, O, S, E]) extends AnyVal with Dsl.Types[F, R, O, S, E] {
    def genActions(name: ANameFn)(g: Gen[Action])(implicit s: Settings): Action =
      TestStateNyaya.genActions(g)(name)(s)

    def genActionsBy(name: ANameFn)(g: ROS => Gen[Action])(implicit s: Settings): Action =
      dsl.chooseAction(name)(i => genActions(name)(g(i))(s))
  }

  def genActions[F[_], R, O, S, E](g: Gen[Actions[F, R, O, S, E]])(name: NameFn[ROS[R, O, S]])(implicit s: Settings): Actions[F, R, O, S, E] =
    g.samples(GenCtx(s.genSize))
      .take(s.sampleSize.value)
      .map(_.groupIfMultipleByLen(_ + " generated actions."))
      .zipWithIndex
      .map(x => x._1.nameMod(n => s"[${x._2 + 1}/${s.sampleSize.value}] ${n.value}"))
      .combine
      .group(name)
}
