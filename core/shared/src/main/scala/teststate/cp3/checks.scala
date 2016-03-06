package teststate.cp3

import teststate.{TriResult, NameFn}
import teststate.{cp3 => ^}
import Profunctor.ToOps._

final case class Point[I, E](name: NameFn[I], test: I => TriResult[E, Unit])

object Point {
  implicit val pointInstanceProfunctor: Profunctor[Point] =
    new Profunctor[Point] {
      override def dimap[A, B, C, D](p: Point[A, B])(g: C => A, f: B => D): Point[C, D] =
        Point(p.name cmap g, c => p.test(g(c)) mapE f)
    }
}


sealed abstract class Around[I, E]
object Around {

  sealed abstract class When
  case object Before         extends When
  case object After          extends When
  case object BeforeAndAfter extends When

  final case class Point[I, E](point: ^.Point[I, E], when: When) extends Around[I, E]

  final case class Delta[I, E](name: NameFn[I], delta: DeltaA[I, E]) extends Around[I, E]

  type DeltaAux[I, E, AA] = DeltaA[I, E] {type A = AA}

  sealed abstract class DeltaA[I, E] {
    type A
    val before: I => TriResult[E, A]
    val test: (I, A) => Option[E]

    final def aux: DeltaAux[I, E, A] = this
  }

  def DeltaA[I, E, AA](_before: I => TriResult[E, AA], _test: (I, AA) => Option[E]): DeltaAux[I, E, AA] =
    new DeltaA[I, E] {
      override type A     = AA
      override val before = _before
      override val test   = _test
    }

  implicit val aroundInstanceProfunctor: Profunctor[Around] =
    new Profunctor[Around] {
      override def dimap[A, B, C, D](around: Around[A, B])(g: C => A, f: B => D): Around[C, D] =
        around match {
          case Point(p, w) => Point(p.dimap(g, f), w)
          case Delta(n, d) =>
            val da = DeltaA[C, D, d.A](
              c => d.before(g(c)) mapE f,
              (c, a) => d.test(g(c), a) map f)
            Delta(n cmap g, da)
        }
    }
}


sealed abstract class Invariant[I, E]
object Invariant {
  final case class Point [I, E](point : ^.Point [I, E]) extends Invariant[I, E]
  final case class Around[I, E](around: ^.Around[I, E]) extends Invariant[I, E]

  implicit val invariantInstanceProfunctor: Profunctor[Invariant] =
    new Profunctor[Invariant] {
      override def dimap[A, B, C, D](a: Invariant[A, B])(g: C => A, f: B => D): Invariant[C, D] =
        a match {
          case Point (x) => Point (x.dimap(g, f))
          case Around(x) => Around(x.dimap(g, f))
        }
    }
}