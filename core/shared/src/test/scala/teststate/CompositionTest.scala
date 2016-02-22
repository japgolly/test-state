package teststate

import utest._
import Show.Implicits.showByToString

object CoproductExample {

  class Txt(init: String) {
    var txt = init
  }

  object Txt {
    val * = Dsl.sync[Txt, String, String, String]

    val txt = *.focus("Txt value").obsAndState(identity, identity)

    def add(a: String) =
      *.action("Add").updateState(_ + a).act(_.ref.txt += a)
        .addCheck(txt.assert.equal.beforeAndAfter)

    val test = Test(add("x").times(3)).observe(_.txt)
  }

  class Num(init: Int) {
    var num = init
  }

  object Num {
    val * = Dsl.sync[Num, Int, Int, String]

    val num = *.focus("Number value").obsAndState(identity, identity)

    def add(a: Int) =
      *.action("Add").updateState(_ + a).act(_.ref.num += a)
        .addCheck(num.assert.equal.beforeAndAfter)

    val test = Test(add(2).times(3)).observe(_.num)
  }

  sealed abstract class Type
  object Type {
    case object Num extends Type
    case object Txt extends Type
    implicit val equality: Equal[Type] = Equal.byUnivEq
  }

  class Top(initNum: Int, initTxt: String) {
    private val num = new Num(initNum)
    private val txt = new Txt(initTxt)

    private var t: Type = Type.Num

    def get(): AnyRef =
      t match {
        case Type.Num => num
        case Type.Txt => txt
      }

    def swap(): Unit =
      t = t match {
        case Type.Num => Type.Txt
        case Type.Txt => Type.Num
      }
  }

  object Top {
    case class State(t: Type, num: Int, txt: String)
    type Obs = Either[Int, String]

    val * = Dsl.sync[Top, Obs, State, String]

    val curType = *.focus("Current type").obsAndState[Type]({
      case Left(_) => Type.Num
      case Right(_) => Type.Txt
    }, _.t)

//    val testNum =
//    - make sure type = num
//      curType.state.assert.equal(Type.Num).before
//    - run Num.test

    val invariants =
      curType.assert.equal

    val test = Test(*.emptyAction, invariants)
      .observe(_.get() match {
        case x: Num => Left(x.num)
        case x: Txt => Right(x.txt)
      })
  }
}

object CompositionTest extends TestSuite {
  override def tests = TestSuite {
    'coproduct {
      import CoproductExample._
      import Top._

      val top = new Top(7, "e")
      val h = test.run(State(Type.Num, 7, "e"), top)
      h.assert(History.Options.colored)
      println(h.format(History.Options.colored.alwaysShowChildren))
      //println(h.format(History.Options.colored))
    }
  }
}
