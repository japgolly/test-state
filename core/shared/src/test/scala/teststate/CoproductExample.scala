package teststate

import teststate.Exports._
import teststate.data.Id

object CoproductExample {

  class Txt(init: String) {
    var txt = init
  }

  object Txt {
    val dsl = Dsl[Txt, String, String]

    val txt = dsl.focus("Txt value").obsAndState(identity, identity)

    def add(a: String) =
      dsl.action("Add")(_.ref.txt += a).updateState(_ + a)
        .addCheck(txt.assert.equal.beforeAndAfter)

    val test = Plan.action(add("x").times(2)).test(Observer(_.txt))
  }

  class Num(init: Int) {
    var num = init
  }

  object Num {
    val dsl = Dsl[Num, Int, Int]

    val num = dsl.focus("Number value").obsAndState(identity, identity)

    def add(a: Int) =
      dsl.action("Add")(_.ref.num += a).updateState(_ + a)
        .addCheck(num.assert.equal.beforeAndAfter)

    val test = Plan.action(add(2).times(2)).test(Observer(_.num))
  }

  sealed abstract class Type {
    final def swap: Type = this match {
      case Type.Num => Type.Txt
      case Type.Txt => Type.Num
    }
  }
  object Type {
    case object Num extends Type
    case object Txt extends Type
    implicit val equality: Equal[Type] = Equal.by_==
  }

  class Top(initNum: Int, initTxt: String) {
    private[this] val num = new Num(initNum)
    private[this] val txt = new Txt(initTxt)

    private var t: Type = Type.Num

    def get(): AnyRef =
      t match {
        case Type.Num => num
        case Type.Txt => txt
      }

    def swap(): Unit =
      t = t.swap
  }

  object Top {
    case class State(t: Type, num: Int, txt: String)
    type Obs = Int Either String

    val dsl = Dsl[Top, Obs, State]

    val curType = dsl.focus("Current type").obsAndState[Type]({
      case Left(_) => Type.Num
      case Right(_) => Type.Txt
    }, _.t)

    val testNum: Actions[Id, Top, Obs, State, String] =
      Num.test.plan
        .mapS[State](_.num)((s, n) => s.copy(num = n))
        .pmapO[Obs] {
            case Left(i) => Right(i)
            case Right(_) => Left("Expected Int, got Txt.")
          }
        .pmapR[Top](_.get() match {
            case x: Num => Right(x)
            case _      => Left("Expected Num, got Txt.")
          })
        .addInvariants(curType.state.assert.equal(Type.Num).before)
        .asAction("Test Num")

    val testTxt: Actions[Id, Top, Obs, State, String] =
      Txt.test.plan
        .mapS[State](_.txt)((s, n) => s.copy(txt = n))
        .pmapO[Obs] {
            case Right(t) => Right(t)
            case Left(_) => Left("Expected Int, got Txt.")
          }
        .pmapR[Top](_.get() match {
            case x: Txt => Right(x)
            case _      => Left("Expected Txt, got Num.")
          })
        .addInvariants(curType.state.assert.equal(Type.Txt).before)
        .asAction("Test Text")

    val invariants =
      curType.assert.equal

    val swapTypes =
      dsl.action("Swap types")(_.ref.swap()).updateStateBy(i => i.state.copy(t = i.state.t.swap))

    val actions: dsl.Actions =
      testNum >> swapTypes >> testTxt

    val test = Plan(actions, invariants).test(
      Observer(_.get() match {
        case x: Num => Left(x.num)
        case x: Txt => Right(x.txt)
      }))
  }
}
