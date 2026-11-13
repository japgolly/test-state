package teststate.dsl

import japgolly.microlibs.name_fn.Name.Implicits._
import japgolly.microlibs.name_fn._
import teststate.data.BeforeAfter
import teststate.typeclass._

// TODO s/implicit d: Display/implicit val display: Display/ in all failure classes

object NameUtils {

  def should(pos: Boolean): String =
    if (pos) "should" else "shouldn't"

  def subjectShouldVerb(focusName: => String, pos: Boolean, verb: => String): Name =
    Name(s"$focusName ${should(pos)} $verb.")

  def equal[A](focusName: => String, pos: Boolean, expect: A)(implicit sa: Display[A]): Name = {
    lazy val _expectedText = sa(expect)
    Name {
      val expectedText = _expectedText
      if (expectedText.contains('\n'))
        s"$focusName ${should(pos)} be:\n  " + expectedText.replace("\n", "\n  ")
      else
        s"$focusName ${should(pos)} be $expectedText."
    }
  }

  def equalFn[I, A](focusName: => String, pos: Boolean, expect: I => A)(implicit sa: Display[A]): Option[I] => Name = {
    case None    => subjectShouldVerb(focusName, pos, "be <?>")
    case Some(i) => equal(focusName, pos, expect(i))
  }

  def equalOptionFn[I, A](focusName: => String, pos: Boolean, expect: I => Option[A])(implicit sa: Display[A]): Option[I] => Name = {
    case None    => subjectShouldVerb(focusName, pos, "be <?>")
    case Some(i) =>
      expect(i) match {
        case Some(a) => equal(focusName, pos, a)
        case None    => subjectShouldVerb(focusName, pos, "be <?>")
      }
  }

  def collChangeFn[I, A](focusName: => String,
                         pos      : Boolean,
                         verb     : => String,
                         expectDel: I => IterableOnce[A],
                         expectAdd: I => IterableOnce[A])
                        (implicit sa: Display[A]): Option[BeforeAfter[I]] => Name = {
    case None    => subjectShouldVerb(focusName, pos, s"$verb: <?>")
    case Some(BeforeAfter(i, _)) =>
      Name.now {
        val del = expectDel(i)
        val add = expectAdd(i)
        val as = del.iterator.map("-" + sa(_)) ++ add.iterator.map("+" + sa(_))
        if (as.isEmpty)
          s"$focusName ${should(!pos)} $verb."
        else
          s"$focusName ${should(pos)} $verb: ${as mkString " "}."
      }
  }
}
