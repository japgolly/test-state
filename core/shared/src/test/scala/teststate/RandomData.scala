package teststate

import nyaya.gen._
import teststate.Exports._
import teststate.core.Action
import teststate.data.Sack

object RandomData {

  val * = Dsl[Unit, Unit, Unit]

  private def applyEndos[A](basic: Gen[A])(endos: (A => A)*): Gen[A] = {
    val id: A => A = identity

    val endo: Gen[A => A] =
      Gen.choose_!(endos)
        .list(0 to 3)
        .map(fs => if (fs.isEmpty) id else fs.reduce(_ compose _))

    for {
      a <- basic
      f <- endo
    } yield f(a)
  }

  lazy val action: Gen[*.Actions] =
    applyEndos(
      Gen.choose(
        *.emptyAction,
        *.action("A")(_ => ()))
    )(
      a => Sack.append(a, a),
      a => Sack.CoProduct("CoA", _ => a),
      a => a.group("AG"),
      a => Action.liftInner(Action.SubTest(a, *.emptyInvariant))("ASub"),
      a => a.skip
    )

  lazy val point: Gen[*.Points] =
    applyEndos(
      Gen.choose(
        *.test("P1")(_ => true),
        *.test("P0")(_ => false))
    )(
      a => Sack.append(a, a),
      a => Sack.CoProduct("CoP", _ => a),
      a => a.skip
    )

}