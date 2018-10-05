package teststate

import utest._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import teststate.data.Id
import Exports._

object StackTest extends TestSuite {

  def test[F[_]](name: String)(implicit EM: ExecutionModel[F]): F[Unit] = {
    val size = Platform.StackTestSize
    val desc = s"${Platform.Name} StackTest.$name (x$size)"
    def time = System.nanoTime()
    val start = time
    val dsl = Dsl.full[F, Unit, Unit, Unit, String]
    val nop = EM.pure(())
    val a = dsl.action("nop")(_ => nop): dsl.Actions
    val as = Iterator.fill(size)(a).reduce(_ >> _)
    val test = Plan.action(as).test(Observer.unit)
    val fh = test.stateless.runU()
    EM.map(fh) { r =>
      assert(r.result == Result.Pass)
      assert(r.history.steps.length == size + 1)
      val end = time
      val t = (end - start).toDouble / 1000000000.0
      println(s"$desc completed in $t s.")
    }
  }

  override def tests = Tests {
    'sync - test[Id]("sync")
    'async - test[Future]("async")
  }
}
