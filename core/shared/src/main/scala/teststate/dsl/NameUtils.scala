package teststate.dsl

import acyclic.file
import scala.collection.compat._
import teststate.data.Name.Implicits._
import teststate.data.{BeforeAfter, Name}
import teststate.typeclass._

// TODO s/implicit d: Display/implicit val display: Display/ in all failure classes

object NameUtils {

  def should(pos: Boolean): String =
    if (pos) "should" else "shouldn't"

  def subjectShouldVerb(focusName: String, pos: Boolean, verb: String): Name =
    s"$focusName ${should(pos)} $verb."

  def equal[A](focusName: String, pos: Boolean, expect: A)(implicit sa: Display[A]): Name =
    subjectShouldVerb(focusName, pos, s"be ${sa(expect)}")

  def equalFn[I, A](focusName: String, pos: Boolean, expect: I => A)(implicit sa: Display[A]): Option[I] => Name = {
    case None    => subjectShouldVerb(focusName, pos, "be <?>")
    case Some(i) => equal(focusName, pos, expect(i))
  }

  def equalOptionFn[I, A](focusName: String, pos: Boolean, expect: I => Option[A])(implicit sa: Display[A]): Option[I] => Name = {
    case None    => subjectShouldVerb(focusName, pos, "be <?>")
    case Some(i) =>
      expect(i) match {
        case Some(a) => equal(focusName, pos, a)
        case None    => subjectShouldVerb(focusName, pos, "be <?>")
      }
  }

  def collChangeFn[I, A](focusName: String,
                         pos      : Boolean,
                         verb     : String,
                         expectDel: I => IterableOnce[A],
                         expectAdd: I => IterableOnce[A])
                        (implicit sa: Display[A]): Option[BeforeAfter[I]] => Name = {
    case None    => s"$focusName ${should(pos)} $verb: <?>."
    case Some(BeforeAfter(i, _)) =>
      val del = expectDel(i)
      val add = expectAdd(i)
      val as = del.iterator.map("-" + sa(_)) ++ add.iterator.map("+" + sa(_))
      if (as.isEmpty)
        s"$focusName ${should(!pos)} $verb."
      else
        s"$focusName ${should(pos)} $verb: ${as mkString " "}."
  }
}
