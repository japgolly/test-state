package teststate

import acyclic.file
import nyaya.gen._
import nyaya.test.Settings
import teststate.data.ROS
import Exports._
import TestStateNyaya._

trait TestStateNyaya {
  implicit def toDslNyayaOps[F[_], R, O, S, E](dsl: Dsl[F, R, O, S, E]): DslNyayaOps[F, R, O, S, E] =
    new DslNyayaOps(dsl)
}

object TestStateNyaya extends TestStateNyaya {

  final class DslNyayaOps[F[_], R, O, S, E](private val dsl: Dsl[F, R, O, S, E]) extends AnyVal {
    def genActions(name: NameFn[ROS[R, O, S]])(g: Gen[Actions[F, R, O, S, E]])(implicit s: Settings): Actions[F, R, O, S, E] =
      TestStateNyaya.genActions(g)(name)(s)
  }

  def genActions[F[_], R, O, S, E](g: Gen[Actions[F, R, O, S, E]])(name: NameFn[ROS[R, O, S]])(implicit s: Settings): Actions[F, R, O, S, E] =
    g.samples(GenCtx(s.genSize))
      .take(s.sampleSize.value)
      .map(_.group("?")) // TODO .groupIfNotGrouped
      .zipWithIndex
      .map(x => x._1.nameMod(n => s"[${x._2 + 1}/${s.sampleSize.value}] ${n.value}"))
      .foldLeft[Actions[F, R, O, S, E]](emptyAction)(_ >> _) // TODO Add flatten action, checks, etc
      .group(name)

}
