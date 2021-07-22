package teststate

import nyaya.gen._
import nyaya.test.Settings
import teststate.Exports._
import teststate.ExtNyaya._
import teststate.data.ROS

trait ExtNyaya {
  implicit def toDslNyayaOps[F[_], R, O, S, E](dsl: Dsl[F, R, O, S, E]): DslNyayaOps[F, R, O, S, E] =
    new DslNyayaOps(dsl)
}

object ExtNyaya extends ExtNyaya {

  final class DslNyayaOps[F[_], R, O, S, E](private val dsl: Dsl[F, R, O, S, E]) extends AnyVal with Dsl.Types[F, R, O, S, E] {
    def genActions(name: ActionName)(g: Gen[Actions])(implicit s: Settings): Actions =
      ExtNyaya.genActions(g)(name)(s)

    def genActionsBy(name: ActionName)(g: ROS => Gen[Actions])(implicit s: Settings): Actions =
      dsl.chooseAction(name)(i => genActions(name)(g(i))(s))
  }

  def genActions[F[_], R, O, S, E](g: Gen[Actions[F, R, O, S, E]])(name: NameFn[ROS[R, O, S]])(implicit s: Settings): Actions[F, R, O, S, E] =
    g.samples(s.genSize)
      .take(s.sampleSize.value)
      .map(_.groupIfMultipleByLen(_.toString + " generated actions."))
      .zipWithIndex
      .map(x => x._1.nameMod(n => s"[${x._2 + 1}/${s.sampleSize.value}] ${n.value}"))
      .combine
      .group(name)
}
