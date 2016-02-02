package teststate

import utest._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object StackTest extends TestSuite {

  def test[F[_]](name: String)(implicit EM: ExecutionModel[F]): F[Unit] = {
    val size = Platform.StackTestSize
    val desc = s"${Platform.Name} StackTest.$name (x$size)"
    def time = System.nanoTime()
    val start = time
    val dsl = Dsl[F, Unit, Unit, Unit, String]
    val nop = EM.pure(())
    val a = dsl.action("nop").act(_ => nop): dsl.Action
    val as = Iterator.fill(size)(a).reduce(_ >> _)
    val test = Test(as).observe(_ => ())
    val fh = test.run((), ())
    EM.map(fh) { h =>
      assert(h.result == Result.Pass)
      assert(h.steps.length == size + 1)
      val end = time
      val t = (end - start).toDouble / 1000000000.0
      println(s"$desc completed in $t s.")
    }
  }

  override def tests = TestSuite {
    'sync - test[Id]("sync")
    'async - test[Future]("async")
  }
}
