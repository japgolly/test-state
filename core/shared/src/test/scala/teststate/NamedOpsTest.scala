package teststate

import cats.instances.string._
import teststate.TestUtil._
import teststate.core._
import teststate.data._
import utest._

object NamedOpsTest extends TestSuite {
  import teststate.Exports._

  override def tests = Tests {

    "thruAfter" - {
      val f: NameFn[BeforeAfter[Int]] => NameFn[BeforeAfter[Int]] =
        _ cmap (ba => BeforeAfter(ba.before + 10, ba.after + 20))

      val n: NameFn[Int] = NameFn(o => s"val=${o.get}")
      val nAfter = f.thruAfter(n)

      assertEq(nAfter(Some(5)).value, "val=25")
    }

    "renameInvariantDelta" - {
      val dsl = Dsl[Unit, Unit, Int]

      val nameFn: dsl.AroundName =
        NameFn(ba => ba.fold("?")(b => s"${b.before.state} -> ${b.after.state}"))

      val delta: dsl.Arounds = dsl.around(nameFn)(_.state)((_, _) => None)
      val inv: dsl.Invariants = delta
      val renamed = inv.prefix("Check: ")

      val deltaA = renamed match {
        case Sack.Value(Right(Invariant.Delta(d))) => d
        case other => sys error s"Expected Invariant.Delta, got $other"
      }
      val name = deltaA.name(Some(BeforeAfter(OS((), 1), OS((), 2)))).value

      assertEq(name, "Check: 1 -> 2")
    }

    "renameCoProduct" - {
      val dsl = Dsl[Unit, Unit, Unit]
      val act = dsl.action("Inner")(_ => ())
      val coproduct: dsl.Actions = dsl.chooseAction("Choice")(_ => act)
      val renamed = coproduct.prefix("P: ")

      val inner = renamed match {
        case Sack.CoProduct(_, p) => p(ROS((), (), ()))
        case other => sys error s"Expected CoProduct, got $other"
      }

      val innerName = inner match {
        case Sack.Value(Right(Action.Outer(name, _, _))) => name(None).value
        case other => sys error s"Expected Outer Action, got $other"
      }
      assertEq(innerName, "P: Inner")
    }

    "renameNamedError" - {
      val ne: NamedError[Failure[String]] Or Action.Outer[Id, Unit, Unit, Unit, String] =
        Left(NamedError(Name.now("Orig"), Failure.NoCause("bad")))

      val renamed = ne.prefix("P: ")

      val actualName = renamed match {
        case Left(NamedError(name, _)) => name.value
        case Right(_) => sys error "Expected Left"
      }

      assertEq(actualName, "P: Orig")
    }

  }
}
