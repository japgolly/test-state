package teststate

import cats.instances.int._
import cats.instances.vector._
import japgolly.microlibs.testutil.TestUtil._
import nyaya.gen.Gen
import nyaya.test.Settings
import teststate.Exports._
import teststate.ExtNyaya._
import utest._

object ExtNyayaTest extends TestSuite {

  override def tests = Tests {

    "genActionsByObservesUpdatedStateAndObsPerStep" - {
      var ref = 0
      var seenStates = Vector.empty[Int]
      var seenObs = Vector.empty[Int]

      val dsl = Dsl[Unit, Int, Int]
      implicit val settings: Settings = Settings().setSampleSize(4)

      val actions = dsl.genActionsBy("gen") { ros =>
        seenStates :+= ros.state
        seenObs :+= ros.obs
        Gen.pure(dsl.action(s"inc from ${ros.state}")(_ => ref += 1).updateState(_ + 1))
      }

      val report = Plan.action(actions).test(Observer.watch(ref)).withInitialState(0).runU()
      report.assert()
      assertEq(ref, 4)
      assertEq(seenStates, Vector(0, 1, 2, 3))
      assertEq(seenObs, Vector(0, 1, 2, 3))
    }

    "genActionsRunsSampleSizeTimes" - {
      var ref = 0
      val dsl = Dsl[Unit, Int, Int]
      implicit val settings: Settings = Settings().setSampleSize(3)

      val actions = dsl.genActions("gen")(Gen.pure(dsl.action("inc")(_ => ref += 1).updateState(_ + 1)))

      val report = Plan.action(actions).test(Observer.watch(ref)).withInitialState(0).runU()
      report.assert()
      assertEq(ref, 3)
    }

  }
}
