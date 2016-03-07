package teststate.data

import utest._

object NameImps extends Name.Implicits
import NameImps._

object NameTest extends TestSuite {

  override def tests = TestSuite {

    'nonStrict {
      'name {
        var i = 0
        val n: Name = s"i = ${ i += 1; i.toString }"
        assert(i == 0)
        assert(n.value == "i = 1")
        assert(i == 1)
        assert(n.value == "i = 1")
        assert(i == 1)
      }

      'nameFn {
        var i = 0
        val f: NameFn[Unit] = s"i = ${ i += 1; i.toString }"
        assert(i == 0)
        assert(f(None).value == "i = 1")
        assert(i == 1)
        assert(f(None).value == "i = 1")
        assert(i == 1)
        assert(f(Some(())).value == "i = 1")
        assert(i == 1)
      }
    }
  }
}
